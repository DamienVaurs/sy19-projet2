# 1. Apprentissage des modèles.
model.phoneme <- model.phoneme
model.robotics <- ...
model.communities <- model_final
# 2. Création des fonctions de prédiction
prediction_phoneme <- function(dataset) {
  library(kernlab)
  predictors <- c(FALSE,FALSE , FALSE , TRUE , TRUE , TRUE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , TRUE , FALSE , TRUE , FALSE , TRUE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , TRUE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , TRUE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , TRUE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , FALSE , TRUE , FALSE , FALSE , FALSE , FALSE , FALSE)
  
  testset <- dataset[,predictors]  
  predict(model.phoneme,newdata=testset,type="response")
}
prediction_robotics <- function(dataset) {
  ...
}
prediction_communities <- function(dataset) {
  prediction_communities <- predict(model, newdata = dataset)
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
