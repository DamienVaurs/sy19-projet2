library(MASS)

set.seed(1998)
x <- read.table("phoneme_train.txt")
y <- as.factor(x$y)

n <- nrow(x)
pca <- princomp(x[,1:p])
x <- cbind(as.data.frame(pca$scores[,1:15]),y)
p <- ncol(x) - 1

KfoldsOuter <- 10 #nb folds CV externe

erreurOuter <- rep(0, KfoldsOuter)
kOuter <- rep(0, KfoldsOuter)
foldsOuter <- sample(1:KfoldsOuter, n, replace=TRUE) 

for (i in 1:KfoldsOuter){
  x.train <- x[foldsOuter != i, ]
  x.test <- x[foldsOuter == i, 1:p]
  y.test <- x[foldsOuter == i, p+1]
  
  fit <- qda(y ~ ., data=x.train)
  pred <- predict(fit, newdata=x.test)$class
  
  erreurOuter[i]<- 1 - mean(pred==y.test)
}


print(erreurOuter) # 0.11737089 0.06866953 0.07407407 0.07079646 0.11914894 0.09019608 0.09952607 0.05670103 0.07211538 0.08620690
print(mean(erreurOuter)) # 0.08548053
print(sd(erreurOuter)) # 0.02109719



