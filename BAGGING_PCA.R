library(rpart)
library(randomForest)

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
  print(i)
  x.train <- x[foldsOuter != i, ]
  x.test <- x[foldsOuter == i, 1:p]
  y.test <- x[foldsOuter == i, p+1]
  
  fit <- randomForest(y ~ ., data=x.train, mtry=p)
  pred <- predict(fit, newdata=x.test, type="response")
  
  erreurOuter[i]<- 1 - mean(pred==y.test)
  print(erreurOuter[i])
}

print(erreurOuter) # 0.14084507 0.09012876 0.08641975 0.08849558 0.12765957 0.08235294 0.10426540 0.08247423 0.08653846 0.11637931
print(mean(erreurOuter)) # 0.1005559
print(sd(erreurOuter)) # 0.02089859


