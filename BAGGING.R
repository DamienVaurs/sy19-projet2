library(randomForest)

set.seed(1998)
x <- read.table("phoneme_train.txt")
y <- as.factor(x$y)

n <- nrow(x)
p <- ncol(x) - 1

x <- cbind(as.data.frame(scale(x[,1:p])), y)

KfoldsOuter <- 10 #nb folds CV externe

erreurOuter <- rep(0, KfoldsOuter)
kOuter <- rep(0, KfoldsOuter)
foldsOuter = sample(1:KfoldsOuter, n, replace=TRUE) 

for (i in 1:KfoldsOuter){
  print(i)
  x.train <- x[foldsOuter != i, ]
  x.test <- x[foldsOuter == i, 1:p]
  y.test <- x[foldsOuter == i, p+1]
  
  fit <- randomForest(y ~ ., data=x.train, mtry=p)
  pred <- predict(fit, newdata=x.test, type="response")
  
  erreurOuter[i]<- 1 - mean(pred==y.test)
  print(erreurOuter[i])
}

print(erreurOuter) # 0.11013216 0.10731707 0.09523810 0.04385965 0.07031250 0.08878505 0.10087719 0.07798165 0.04761905 0.06008584
print(mean(erreurOuter)) # 0.08022083
print(sd(erreurOuter)) # 0.02414784
