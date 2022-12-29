library(randomForest)

set.seed(1998)
x <- read.table("phoneme_train.txt")
y <- as.factor(x$y)

n <- nrow(x)
p <- ncol(x) - 1

x <- cbind(as.data.frame(scale(x[,1:p])), y)


nb_m <- c(8)

KfoldsOuter <- 10 #nb folds CV externe
KfoldsInner <- 5 #nb folds CV interne

erreurOuter <- rep(0, KfoldsOuter)
mOuter <- rep(0, KfoldsOuter)
foldsOuter = sample(1:KfoldsOuter, n, replace=TRUE) 

for (i in 1:KfoldsOuter){
  print(i)
  x.train <- x[foldsOuter != i, ]
  x.test <- x[foldsOuter == i, 1:p]
  y.test <- x[foldsOuter == i, p+1]
  
  foldsInner <- sample(1:KfoldsInner, nrow(x.train), replace=TRUE)
  erreur.inner <- rep(0, length(nb_m))
  
  for (m in 1:length(nb_m)){
    print(nb_m[m])
    for (j in 1:KfoldsInner) { # Cross validation interne
      print(j)
      x.train.inner <- x.train[foldsInner != j, ]
      x.test.inner <- x.train[foldsInner == j, 1:p]
      y.test.inner <- x.train[foldsInner == j, p+1]
      
      fit.inner <- randomForest(y ~ ., data = x.train.inner, mtry = nb_m[m])
      pred.inner <- predict(fit.inner, newdata=x.test.inner, type="response")
      
      err <- 1 - mean(pred.inner == y.test.inner)
      erreur.inner[m]<- erreur.inner[m] + err
      
    }
    erreur.inner[m] <- erreur.inner[m]/KfoldsInner
  }
  
  
  mOuter[i]<- nb_m[which.min(erreurInner)]
  print(mOuter[i])
  
  fit <- randomForest(y ~ ., data=x.train, mtry=mOuter[i])
  pred <- predict(fit, newdata=x.test, type="response")
  
  erreurOuter[i]<- 1 - mean(pred==y.test)
  print(erreurOuter[i])
}


print(erreurOuter) # 0.13615023 0.08583691 0.09876543 0.08849558 0.12340426 0.09411765 0.08056872 0.08762887 0.09615385 0.09913793
print(mean(erreurOuter)) # 0.09902594
print(sd(erreurOuter)) # 0.01751297
