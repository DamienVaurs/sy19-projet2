

# install.packages("visdat")
# install.packages("ggplot2")
library(visdat)
library(ggplot2)
library(randomForest)
library(missForest)
library(mice)
library(caret)
library(mclust)
library(nnet)
library(mgcv)
library(e1071)
library(MASS)
library(glmnet)
library(leaps)
library(rpart)
set.seed(123)

# Missing values article --> https://journalofbigdata.springeropen.com/articles/10.1186/s40537-021-00516-9
# Function missingno


data <- read.csv("communities_train.csv")
tab_mse <- table(x = "modele", y = "mse")

# Jeu de données sans les variables non prédictives (utilisé pour la suite)
data_without_not_predictive <- data[, -(1:5)]

# Decommenter pour voir le nombre de données manquantes par variable
# sapply(data_without_not_predictive, function(x) sum(is.na(x)))

# Jeu de données dont les données manquantes ont été estimées à l'aide d'une forêt aléatoire
data_imputed <- missForest(data_without_not_predictive)$ximp 

# Jeu de données sans les observations incomplètes
data_complete <- data_without_not_predictive[complete.cases(data_without_not_predictive), ]

# Jeu de données sans les variables comprenant des observations incomplètes
data_complete2 <- data_without_not_predictive[, colSums(is.na(data_without_not_predictive)) == 0]

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







################### Modèles de prédiction #####################


split <- createDataPartition(y = data_complete2$ViolentCrimesPerPop, p = 0.9, list = FALSE)
data_train <- data_complete2[split, ]
data_test <- data_complete2[-split, ]

split <- createDataPartition(y = data_without_not_predictive$ViolentCrimesPerPop, p = 0.9, list = FALSE)
data_train2 <- data_without_not_predictive[split, ]
data_test2 <- data_without_not_predictive[-split, ]

X_train <- subset(data_train, select = -ViolentCrimesPerPop)
y_train <- data_train$ViolentCrimesPerPop
X_test <- subset(data_test, select = -ViolentCrimesPerPop)
y_test <- data_test$ViolentCrimesPerPop

liste_mse <- list()

# Cross-validation avec 10 plis
cv <- trainControl(method = "cv", number = 10)
metric <- "MSE"

fit <- regsubsets(ViolentCrimesPerPop ~ ., data = data_train, really.big = T)
summary(fit)
plot(fit, scale = "Cp")


################### Prédiction avec Mclust #####################

#model <- Mclust(data_train, G = 4)
#clusters <- predict(model, data_train)

################### Prédiction avec régression linéaire #####################

model <- lm(ViolentCrimesPerPop ~ ., data = data_train)
summary(model)
predictions <- predict(model, newdata = data_test)
mse <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse)
liste_mse <- c(liste_mse, list(RegLinéaire = mse))
tab_mse <- rbind(modele = "RegLinéaire", mse = mse)

results <- train(ViolentCrimesPerPop ~ ., data = data_complete2, method = "lm", trControl = cv, metric = metric)
print(results)

################### Prédiction avec forêt aléatoire #####################

model <- randomForest(ViolentCrimesPerPop ~ ., data = data_train)
summary(model)
predictions <- predict(model, newdata = data_test)
mse <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse)
liste_mse <- c(liste_mse, list(RandomForest = mse))
tab_mse <- rbind(modele = "RandomForest", mse = mse)

model <- train(ViolentCrimesPerPop ~ ., data = data_train, method = "rf", trControl = trainControl(method = "cv", number = 10))
predictions <- predict(model, newdata = data_test)
mse <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse)
liste_mse <- c(liste_mse, list(RandomForestCV = mse))
tab_mse <- rbind(modele = "RandomForestCV", mse = mse)

################### Prédiction avec réseau de neurones #####################

model <- nnet(ViolentCrimesPerPop ~ ., data = data_train, size = 6, decay = 0.1)
summary(model)
predictions <- predict(model, newdata = data_test)
mse <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse)
liste_mse <- c(liste_mse, list(NeuralNetwork = mse))

################### Prédiction avec GAM #####################

variables <- names(data_train)
variables <- variables[variables != "ViolentCrimesPerPop"]
formula <- paste("ViolentCrimesPerPop ~", paste(variables, collapse = " + "))
model <- gam(as.formula(formula), data = data_train)
print(model)
predictions <- predict(model, newdata = data_test)
mse <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse)
liste_mse <- c(liste_mse, list(GAM = mse))

#k <- 10
#results <- cv.gam(data_train, formula, k = k)

#results <- train(formula, data = data_train, method = "gam", trControl = cv, metric = metric)
#print(results)

################### Prédiction avec Regression Lasso #####################

model <- glmnet(x = X_train, y = y_train, alpha = 1)
predictions <- predict(model, newx = as.matrix(X_test))
mse <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse)
liste_mse <- c(liste_mse, list(Lasso1 = mse))

cv.out <- cv.glmnet(as.matrix(X_train), y_train, alpha=1, standardize=TRUE)
model <- glmnet(X_train, y_train, lambda=cv.out$lambda.min, alpha=1, standardize=TRUE)
predictions <- predict(model, s=cv.out$lambda.min, newx = as.matrix(X_test))
mse <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse)
liste_mse <- c(liste_mse, list(Lasso2 = mse))

################### Prédiction avec Regression Ridge #####################

model <- glmnet(as.matrix(X_train), y_train, alpha = 0, lambda = 0.1)
predictions <- predict(model, newx = as.matrix(X_test))
mse <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse)
liste_mse <- c(liste_mse, list(Ridge1 = mse))

cv.out <- cv.glmnet(as.matrix(X_train), y_train, alpha=0, standardize=TRUE)
model <- glmnet(X_train, y_train, lambda=cv.out$lambda.min, alpha=0, standardize=TRUE)
predictions <- predict(model, s=cv.out$lambda.min, newx = as.matrix(X_test))
mse <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse)
liste_mse <- c(liste_mse, list(Ridge2 = mse))

################### Prédiction avec Regression Ridge et critère BIC #####################

model <- glmnet(X_train, y_train, alpha = 0, nlambda = 100, lambda.min.ratio = 0.0001, standardize = TRUE, intercept = TRUE, score.response = "bic")
predictions <- predict(model, newx = as.matrix(X_test))
mse <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse)
liste_mse <- c(liste_mse, list(RidgeBIC = mse))

################### Prédiction avec Regression Lasso et critère BIC #####################

model <- glmnet(X_train, y_train, alpha = 1, standardize = TRUE, intercept = TRUE, score.response = "bic")
predictions <- predict(model, newx = as.matrix(X_test))
mse <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse)
liste_mse <- c(liste_mse, list(LassoBIC = mse))

################### Prédiction avec arbre de régression #####################

model <- rpart(ViolentCrimesPerPop ~ ., data = data_train, control = rpart.control(xval = 10, minbucket = 10, cp = 0))
predictions <- predict(model, newdata = data_test)
mse <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse)
#plotcp(model)
#printcp(model)
liste_mse <- c(liste_mse, list(RegTree = mse))

i.min <- which.min(model$cptable[,4])
cp.opt <- model$cptable[i.min,1]
pruned_tree <- prune(model, cp=cp.opt)
predictions <- predict(pruned_tree, newdata = data_test)
mse <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse)
liste_mse <- c(liste_mse, list(PrunedTree = mse))

################### Prédiction avec subset selection variables #####################

model <- regsubsets(ViolentCrimesPerPop ~.,data=data_train, method='forward', nvmax=30)
res.forward <- summary(model)
best <- which.min(res.forward$bic)
ntst <- nrow(as.matrix(X_test))
X <- cbind(rep(1,ntst),as.matrix(X_test))
ypred<-X[,res.forward$which[best,]]%*%coef(model,best)
mse <-mean((ypred-data_test$ViolentCrimesPerPop)^2)
liste_mse <- c(liste_mse, list(SubSelVar = mse))



model_final <- randomForest(ViolentCrimesPerPop ~ ., data = data_complete2)
summary(model)

keys <- names(liste_mse)
values <- unname(liste_mse)
plot(t(keys), values, type="b", xlab="Clés", ylab="Valeurs")
plot(unlist(liste_mse))


model_names
ggplot(df, aes(x=model_names, y=mse_values)) + geom_bar(stat="identity")

















