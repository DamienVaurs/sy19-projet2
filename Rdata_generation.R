# 1. Apprentissage des modèles.
model.phoneme <- ...
model.robotics <- ...
model.communities <- ...
# 2. Création des fonctions de prédiction
prediction_phoneme <- function(dataset) {
  # Ne pas oublier de charger **à l’intérieur de la fonction** les
  # bibliothèques utilisées.
  library(...)
  # Attention à ce que retourne un modèle en prédiction. Par exemple,
  # la lda retourne une liste nommée. On sélectionne alors les
  # classes.
  predict(clas, test_set)$class
}
prediction_robotics <- function(dataset) {
  ...
}
prediction_communities <- function(dataset) {
  ...
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
