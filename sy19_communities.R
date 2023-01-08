

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
library(corrplot)
library(graphics)
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

vis_miss(subset_data,warn_large_data = FALSE,sort_miss = TRUE) + theme(text = element_text(size=10),axis.text.x = element_text(angle=90,hjust=0)) +
                                                                         labs(title="Diagramme des données manquantes", x="Prédicteurs", y="Observations") +
  theme(plot.title = element_text(hjust = 0.5))



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

make_one_formula <- function(names) {
  # Création de la formule correspondante au modèle i
  f <- "y ~"
  for (name in names) {
    f <- paste(f, "+", name)
  }
  return (f)
}


make_formula <- function(reg.fit) {
  
  Formula <- c()
  for (i in 2:length(summary(reg.fit)$which[1,])-1) {
    
    model <- summary(reg.fit)$which[i,] # Masque du modèle i
    model <- model[2:length(model)] # On ne prend pas la colone (Intercept)
    model.names <- names(model)[model]
    
    names <- model.names[1:length(model.names)]
    f <- make_one_formula(names)
    
    # Ajout de la formule à la liste des formules
    Formula <- append(Formula, f)
  }
  
  return (Formula)
}


n1 <- nrow(data_train)
K1 <- 5
K2 <- 10
folds1 <- sample(1:K1, n1, replace=TRUE)
CV1 <- rep(0, length(Formula))
for (k1 in (1:K1)){
  inner.data <- data_train[folds1 != k1, ]
  n2 <- nrow(inner.data)
  folds2 <- sample(1:K2, n2, replace=TRUE)
  CV2 <- rep(0, length(Formula))
  for(i in (1:length(Formula))) {
    for(k2 in (1:K2)) {
      reg <- lm(Formula[i], data=inner.data[folds2 != k2, ])
      pred <- predict(reg, newdata=inner.data[folds2 == k2, ])
      CV2[i] <- CV2[i] + sum((inner.data$y[folds2 == k2] - pred)^2)
    }
    CV2[i] <- CV2[i]/n2
  }
  CV1 <- CV1 + CV2
}
CV1 <- CV1/K1

par(mfrow = c(1, 2))
sol <- which.min(CV1)
plot(CV1, type="b")




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

lm.reg.fit <- regsubsets(ViolentCrimesPerPop~., data=data_train, method='backward', nvmax=length(data_train)-1)
Formula <- make_formula(lm.reg.fit)


################### Prédiction avec Mclust #####################

#model <- Mclust(data_train, G = 4)
#clusters <- predict(model, data_train)

################### Prédiction avec régression linéaire #####################

model <- lm(ViolentCrimesPerPop ~ ., data = data_train)
summary(model)
predictions <- predict(model, newdata = data_test)
mse3 <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse3)

results <- train(ViolentCrimesPerPop ~ ., data = data_train, method = "lm", trControl = cv, metric = metric)
predictions <- predict(model, newdata = data_test)
mse4 <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse4)
liste_mse <- c(liste_mse, list(RegLinéaire = min(mse3, mse4)))
print(results)

################### Prédiction avec forêt aléatoire #####################

model <- randomForest(ViolentCrimesPerPop ~ ., data = data_train)
summary(model)
predictions <- predict(model, newdata = data_test)
mse1 <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse1)



model <- train(ViolentCrimesPerPop ~ ., data = data_train, method = "rf", trControl = trainControl(method = "cv", number = 10))
print(model)
predictions <- predict(model, newdata = data_test)
mse2 <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse2)
liste_mse <- c(liste_mse, list(RandomForestCV = min(mse1, mse2)))



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
mse5 <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse5)

cv.out <- cv.glmnet(as.matrix(X_train), y_train, alpha=1, standardize=TRUE)
model <- glmnet(X_train, y_train, lambda=cv.out$lambda.min, alpha=1, standardize=TRUE)
predictions <- predict(model, s=cv.out$lambda.min, newx = as.matrix(X_test))
mse6 <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse6)
liste_mse <- c(liste_mse, list(Lasso = min(mse5, mse6)))

################### Prédiction avec Regression Ridge #####################

model <- glmnet(as.matrix(X_train), y_train, alpha = 0, lambda = 0.1)
predictions <- predict(model, newx = as.matrix(X_test))
mse7 <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse7)

cv.out <- cv.glmnet(as.matrix(X_train), y_train, alpha=0, standardize=TRUE)
model <- glmnet(X_train, y_train, lambda=cv.out$lambda.min, alpha=0, standardize=TRUE)
predictions <- predict(model, s=cv.out$lambda.min, newx = as.matrix(X_test))
mse8 <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse8)

cv.out <- cv.glmnet(as.matrix(X_train), y_train, alpha=0)
plot(cv.out)
cv.out$lambda.min
model <- glmnet(X_train, y_train, lambda=cv.out$lambda.min, alpha=0)
predictions <- predict(model, s=cv.out$lambda.min, newx=as.matrix(X_test))
mse9 <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse9)
liste_mse <- c(liste_mse, list(Ridge = min(mse7,mse8,mse9)))


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

################### Prédiction avec Regression elastic net #####################

cv.out <- cv.glmnet(as.matrix(X_train), y_train, alpha=0.5)
plot(cv.out)
cv.out$lambda.min
model <- glmnet(X_train, y_train, lambda=cv.out$lambda.min, alpha=0.5)
predictions <- predict(model, s=cv.out$lambda.min, newx=as.matrix(X_test))
mse <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse)
liste_mse <- c(liste_mse, list(ElasticNet = mse))

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

lm.reg.fit.summary <- summary(lm.reg.fit)
bic <- lm.reg.fit.summary$outmat[which.min(lm.reg.fit.summary$bic), ]
bic.predicteurs <- names(data_train)[bic == "*"]
bic.predicteurs <- bic.predicteurs[-length(bic.predicteurs)] # Attention  enlever le y de la formule lorsque le backward est utilis
plot(lm.reg.fit, scale="bic")  # 46 predicteurs
f <- make_one_formula(bic.predicteurs)
model <- lm(f, data_train)
predictions <- predict(model, newdata=data_test)
mse <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse)

################### Prédiction avec SVM #####################

model <- svm(ViolentCrimesPerPop ~ ., data = data_train, type = "eps-regression")
predictions <- predict(model, newdata = data_test)
mse10 <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse10)

model <- train(ViolentCrimesPerPop ~ ., data = data_train, method = "svmRadial", trControl = trainControl(method = "cv", number = 10))
print(model)
predictions <- predict(model, newdata = data_test)
mse11 <- mean((predictions - data_test$ViolentCrimesPerPop)^2)
print(mse11)
liste_mse <- c(liste_mse, list(SVM = min(mse10, mse11)))


model_final <- train(ViolentCrimesPerPop ~ ., data = data_complete2, method = "rf", trControl = trainControl(method = "cv", number = 10))
print(model_final)
summary(model_final)



df <- stack(liste_mse)
df$Legende <- ifelse(df$values == min(df$values), "lowest", "not_lowest")
ggplot(df, aes(x = ind, y = values, , fill=Legende)) + geom_bar(stat = "identity")+
  labs(title="Diagramme en barres des MSE de chaque modèle étudié", x="Modèle", y="MSE") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("lowest" = "red", "not_lowest" = "blue"), labels = c("Valeur la plus basse", "Autres valeurs"))

















