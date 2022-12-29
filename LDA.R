library(FactoMineR)
library(factoextra)
library(MASS)
library(caret)

set.seed(1998)
x <- read.table("phoneme_train.txt")
y <- as.factor(x$y)

n <- nrow(x)
p <- ncol(x) - 1

x <- cbind(as.data.frame(scale(x[,1:p])), y)

plot(x$y)

head(x)

# On effectue dans un premier temps une LDA sur les données complètes afin de voir ce que ça donne
# niveau performances, ensuite KNN (on commence par des méthodes basiques)

# Il faut voir les performances que l'on obtient avec toutes les données, et si on a des soucis
# Faire alors une PCA pour réduire le nombre de features (PCA doit etre JUSTIFIEE)
# Visualiser les données en fonction du temps
# Effectuer une analyse approfondie : Etudier l'influence de chaque facteur sur le résultat, 
# expliquer pourquoi certaines méthodes marchent bien, d'autres non



# Nous allons effectuer une validation croisée à 10 plis pour les méthodes suivantes:
# - LDA
# - K plus proches voisins (sans PCA puis AVEC PCA si trop de voisins)
# - Modèle de mélange
# - Bayésien Naïf ? 
# - Bagging et forêts aléatoires (PCA indispensable)

## LDA 
# Pour faire LDA, il faut vérifier l'égalité des covariances (matrice de covariance commune pour toutes les classes) 
KfoldsOuter <- 10 #nb folds CV externes

erreurOuter <- rep(0, KfoldsOuter)
kOuter <- rep(0, KfoldsOuter)
foldsOuter = sample(1:KfoldsOuter, n, replace=TRUE) 

for (i in 1:KfoldsOuter){
  x.train <- x[foldsOuter != i, ]
  x.test <- x[foldsOuter == i, 1:p]
  y.test <- x[foldsOuter == i, p+1]
  
  fit <- lda(y ~ ., data=x.train)
  pred <- predict(fit, newdata=x.test)$class
  
  erreurOuter[i]<- 1 - mean(pred==y.test)
  
  confusionMatrix(as.factor(x.test), pred)
}

print(erreurOuter) # 0.06437768 0.07246377 0.06557377 0.07296137 0.10714286 0.06521739 0.06603774 0.09282700 0.05504587 0.11956522
print(mean(erreurOuter)) # 0.07812127
print(sd(erreurOuter)) # 0.02115043

err.lda<-matrix(0, 10, ncol=length(n))
lda.mean <- colMeans(err.lda)

# LDA donne une erreur moyenne très faible (~0.07) sur les données totales (sans PCA)
# On espère avoir encore de meilleurs résultats après une PCA, car LDA est habituellement très performante pour un plus petit
# nombre de paramètres
# LDA présente un taux d'erreur assez faible, ce qui laisse penser que l'hypothèse d'homocédasticité est réaliste. 

# Erreur théorique de Bayes
errb<-mean(y.test != pred)
errb


plot(1:10, erreurOuter, type='l', ylim=c(0.03, 0.15), ylab="Erreur de test")
abline(h=errb,col="red")
legend("bottomleft", legend="LDA", col="blue", lty = 1:2)


