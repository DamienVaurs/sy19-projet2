library(FactoMineR)
library(factoextra)
library(MASS)

set.seed(1998)
x <- read.table("phoneme_train.txt")
y <- as.factor(x$y)

n <- nrow(x)
p <- ncol(x) - 1

pca <- princomp(x[,1:p])
x <- cbind(as.data.frame(pca$scores[,1:15]),y)
p <- ncol(x) - 1


KfoldsOuter <- 10 #nb folds CV externe

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
}

print(erreurOuter) # 0.10328638 0.06866953 0.06172840 0.05309735 0.10638298 0.09411765 0.08530806 0.07216495 0.06730769 0.06465517 
print(mean(erreurOuter)) # 0.07767181
print(sd(erreurOuter)) # 0.01842561
