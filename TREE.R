library(rpart)
library(rpart.plot)

set.seed(1998)
x <- read.table("phoneme_train.txt")
y <- as.factor(x$y)

n <- nrow(x)
p <- ncol(x) - 1

x <- cbind(as.data.frame(scale(x[,1:p])), y)

KfoldsOuter <- 10 #nb folds CV externe
KfoldsInner <- 5

erreurOuter <- rep(0, KfoldsOuter)
kOuter <- rep(0, KfoldsOuter)
folds.outer <- sample(1:KfoldsOuter, n, replace=TRUE) 

for (i in 1:KfoldsOuter){
  print(i)
  
  x.train <- x[folds.outer != i,]
  
  folds.inner <- sample(nrow(x.train), nrow(x.train) - as.integer(nrow(x.train)/KfoldsInner))
  
  
  fit <- rpart(formula = y ~ . , data = x.train, subset = folds.inner, method = "class",
               control = rpart.control(xval = KfoldsInner,minbucket = 10,cp=0.00))
  
  pred <- predict(fit, newdata=x.train[-folds.inner,], type="class")
  
  erreurOuter[i]<- 1 - mean(pred == x.train[-folds.inner, p+1])
  print(erreurOuter[i])
}

print(erreurOuter) # 0.1100244 0.1572482 0.1369193 0.1194030 0.1820449 0.1530864 0.1410891 0.1265509 0.1485149 0.1321696
print(mean(erreurOuter)) # 0.1407051
print(sd(erreurOuter)) # 0.02076241

rpart.plot(fit, box.palette="RdBu", shadow.col="gray",
           fallen.leaves=FALSE)

plotcp(fit)



