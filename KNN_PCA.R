library(FNN)

set.seed(1998)
x <- read.table("phoneme_train.txt")
y <- as.factor(x$y)

n <- nrow(x)
p <- ncol(x) - 1

pca <- princomp(x[,1:p])
x <- cbind(as.data.frame(pca$scores[,1:20]),y)
p <- ncol(x) - 1
fviz_eig(pca, main="Pourcentage de variance expliquée par les composantes principales", barfill="#ff4305", barcolor="#ff4305", ncp = 15, addlabels = TRUE, ylim = c(0, 60))

Kmax <- 30 #nb voisins max

KfoldsOuter <- 10 #nb folds CV externe
KfoldsInner <- 5 #nb folds CV interne

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
    
    for (j in 1:KfoldsInner) { # Cross validation interne
      
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
  
  erreurOuter[i]<- 1 - mean(fit == y.test)
  print(erreurOuter[i])
}

print(erreurOuter) # 0.10798122 0.07296137 0.08641975 0.07964602 0.11489362 0.09411765 0.08056872 0.07216495 0.08653846 0.08189655
print(mean(erreurOuter)) # 0.08771883
print(sd(erreurOuter)) #  0.01415375


