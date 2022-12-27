

# install.packages("visdat")
# install.packages("ggplot2")
library(visdat)
library(ggplot2)
library(randomForest)
library(missForest)
library(mice)
library(missingno)

# Missing values article --> https://journalofbigdata.springeropen.com/articles/10.1186/s40537-021-00516-9
# Function missingno


data <- read.csv("communities_train.csv")

# Jeu de données sans les variables non prédictives (utilisé pour la suite)
data_without_not_predictive <- data[, -(1:5)]

# Decommenter pour voir le nombre de données manquantes par variable
# sapply(data_without_not_predictive, function(x) sum(is.na(x)))

# Jeu de données dont les données manquantes ont été estimées à l'aide d'une forêt aléatoire
data_imputed <- missForest(data_without_not_predictive)$ximp 

# Jeu de données sans les observations incomplètes
data_complete <- data[complete.cases(data), ]

# Jeu de données sans les variables comprenant des observations incomplètes
data_complete2 <- data[, colSums(is.na(data)) == 0]

# Données manquantes estimées grâce à MICE (methode norm)
data_mice <- data_without_not_predictive
init = mice(data_mice, maxit=0) 
meth = init$method
predM = init$predictorMatrix
meth[] = "norm"
set.seed(103)
imputed = mice(data_mice, method=meth, predictorMatrix=predM, m=5)
data_mice <- complete(imputed)
sapply(data_mice, function(x) sum(is.na(x)))

# Données manquantes estimées grâce à MICE
data_mice2 <- data_without_not_predictive
init = mice(data_mice2, maxit=0) 
meth = init$method
predM = init$predictorMatrix
#meth[] = "norm"
set.seed(103)
imputed = mice(data_mice2, method=meth, predictorMatrix=predM, m=5)
data_mice2 <- complete(imputed)
sapply(data_mice2, function(x) sum(is.na(x)))

# Données manquantes remplacées par la moyenne des autres données
data_mean <- data_without_not_predictive
column_means <- sapply(data_mean, mean, na.rm = TRUE)
for (col in colnames(data_mean)) {
  data_mean[, col] <- ifelse(is.na(data_mean[, col]), column_means[col], data_mean[, col])
}

# Données manquantes remplacées par la médiane des autres données
data_med <- data_without_not_predictive
column_meds <- sapply(data_med, median, na.rm = TRUE)
for (col in colnames(data_med)) {
  data_med[, col] <- ifelse(is.na(data_med[, col]), column_meds[col], data_med[, col])
}


################### Visualisation des données manquantes #####################

na_columns <- sapply(data_without_not_predictive, is.na)
na_counts <- colSums(na_columns)
selected_columns <- na_counts > 0
subset_data <- subset(data_without_not_predictive, select = selected_columns)

vis_miss(subset_data,warn_large_data = FALSE,sort_miss = TRUE) + theme(text = element_text(size=10),axis.text.x = element_text(angle=90,hjust=0))



################### Données manquantes par régression linéaire #####################
# (ne fonctionne pas) #

cols_with_missing_values <- which(sapply(data_without_not_predictive, function(x) any(is.na(x))))

for (col in cols_with_missing_values) {
  not_missing <- !is.na(data_without_not_predictive[, col])
  x <- data_without_not_predictive[not_missing, -col]
  y <- data_without_not_predictive[not_missing, col]
  model <- lm(y ~ ., data = x)
}

missing <- is.na(data_without_not_predictive[, col])
z <- data_without_not_predictive[missing, -col]
options(warn=-1)
predictions <- predict(model, newdata = z)
data_without_not_predictive[missing, col] <- predictions


