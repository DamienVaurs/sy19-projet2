library(mda)

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
  
  fit <- fda(y ~ ., data=x.train)
  pred <- predict(fit, newdata=x.test)
  
  erreurOuter[i]<- 1 - mean(pred==y.test)
  print(erreurOuter[i])
}

print(erreurOuter) # 
print(mean(erreurOuter)) # 
print(sd(erreurOuter)) # 
