library(caret)
set.seed(5)
tuneGrid <- expand.grid(
  C = c(0.25, 0.5, 1),
  sigma = 0.1
)
ctrl <- trainControl(method = "cv", number = 10)

svr.model <- train(y~., data=read.table('robotics_train.txt'), method="svmRadial", trcotrol=ctrl, tuneGrid = tuneGrid)

svr.model.pred <- predict(svr.model, test)
error <- svr.model.pred - test[["y"]]
MSE <- mean(error^2)
print(MSE)


# 1. Apprentissage des modèles.
model.phoneme <- model.phoneme
model.robotics <- svr.model
model.communities <- model_final
# 2. Création des fonctions de prédiction
prediction_phoneme <- function(dataset) {
  library(kernlab)
  predictors <- c(FALSE,FALSE , FALSE , TRUE , TRUE , TRUE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , TRUE , FALSE , TRUE , FALSE , TRUE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , TRUE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , TRUE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , TRUE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE)
  
  testset <- dataset[,predictors]  
  predict(model.phoneme,newdata=testset,type="response")
}
prediction_robotics <- function(dataset) {
  prediction_robotics <- predict(model.robotics, newdata = dataset)
}
prediction_communities <- function(dataset) {
  library(randomForest)
  prediction_communities <- predict(model.communities, newdata = dataset)
}
# 3. Sauvegarder sous forme de fichier .Rdata les fonctions
# ’prediction_phoneme’, ’prediction_robotics’, ’prediction_communities’.
# Sauvegarder également les objets utilisés dans ces fonctions
# (‘model.phoneme‘, ‘model.robotics‘ et ‘model.communities‘ dans l’exemple) !
save(
  "model.phoneme",
  "model.robotics",
  "model.communities",
  "prediction_phoneme",
  "prediction_robotics",
  "prediction_communities",
  file = "env.Rdata"
)
