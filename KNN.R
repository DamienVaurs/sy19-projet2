library(FactoMineR)
library(factoextra)
library(MASS)
library(FNN)

set.seed(1998)
x <- read.table("phoneme_train.txt")
y <- as.factor(x$y)

n <- nrow(x)
p <- ncol(x) - 1

x <- cbind(as.data.frame(scale(x[,1:p])), y)

# On essaye ensuite de faire de la KNN. On peut utiliser KNN car c'est de l'apprentissage supervisé. 
# On effectue la KNN, puis on calcule le taux d'erreur :
Kmax <- 30 # Nombre max de voisins

KfoldsOuter <- 10 #nb folds CV externes
KfoldsInner <- 5 #nb folds CV internes

erreurOuter <- rep(0, KfoldsOuter)
kOuter <- rep(0, KfoldsOuter)
foldsOuter = sample(1:KfoldsOuter, n, replace=TRUE) 

for (i in 1:KfoldsOuter){
  print(i)
  x.train <- x[foldsOuter != i, 1:p]
  y.train <- x[foldsOuter != i, p+1]
  x.test <- x[foldsOuter == i, 1:p]
  y.test <- x[foldsOuter == i, p+1]
  
  foldsInner = sample(1:KfoldsInner, nrow(x.train), replace=TRUE)
  erreurInner <- rep(0, Kmax)
  
  for (k in 1:Kmax){
    
    for (j in 1:KfoldsInner) { # CV interne
      
      x.train.inner <- x.train[foldsInner != j,]
      y.train.inner <- y.train[foldsInner != j]
      x.test.inner <- x.train[foldsInner == j,]
      y.test.inner <- y.train[foldsInner == j]
      
      fit.inner <- knn(train=x.train.inner, test = x.test.inner, cl= y.train.inner, k = k)
      err <- 1 - mean(fit.inner == y.test.inner)
      erreurInner[k]<- erreurInner[k]+ err
      
    }
    erreurInner[k] <- erreurInner[k]/KfoldsInner
  }
  
  idx.kmin <-  which.min(erreurInner)
  kOuter[i]<- idx.kmin
  
  fit <- knn(train=x.train, test = x.test, cl= y.train, k = idx.kmin)
  
  erreurOuter[i]<- 1 - mean(fit == y.test))
}

print(erreurOuter) # 0.12807882 0.10666667 0.10185185 0.09954751 0.09787234 0.08085106 0.10833333 0.09871245 0.08219178 0.08071749 
print(mean(erreurOuter)) # 0.09848233
print(sd(erreurOuter))# 0.01469968




