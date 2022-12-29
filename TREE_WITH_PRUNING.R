library(rpart)
library(rpart.plot)

set.seed(1998)
x <- read.table("phoneme_train.txt")
y <- as.factor(x$y)

n <- nrow(x)
p <- ncol(x) - 1

x <- cbind(as.data.frame(scale(x[,1:p])), y)

KfoldsOuter <- 10 #nb folds CV externe
KfoldsInner <-10

erreurOuter <- rep(0, KfoldsOuter)
kOuter <- rep(0, KfoldsOuter)
folds.outer <- sample(1:KfoldsOuter, n, replace=TRUE) 

for (i in 1:KfoldsOuter){
  print(i)
  
  x.train <- x[folds.outer != i,]
  
  folds.inner <- sample(nrow(x.train), nrow(x.train) - as.integer(nrow(x.train)/KfoldsInner))
  
  
  fit <- rpart(formula = y ~ . , data = x.train, subset = folds.inner, method = "class",
               control = rpart.control(xval = KfoldsInner,minbucket = 10,cp=0.00))
  
  i.min<-which.min(fit$cptable[,4])
  cp.opt1<-fit$cptable[i.min,1]
  print(cp.opt1)
  pruned_tree <- prune(fit, cp=cp.opt1)
  
  pred <- predict(pruned_tree, newdata=x.train[-folds.inner,], type="class")
  
  erreurOuter[i]<- 1 - mean(pred == x.train[-folds.inner, p+1])
  print(erreurOuter[i])
}

print(erreurOuter) # 0.1274510 0.1492537 0.1133005 0.1633663 0.1691542 0.1393035 0.1194030 0.1435644 0.1584158 0.1219512
print(mean(erreurOuter)) # 0.1405164
print(sd(erreurOuter)) # 0.01958341

rpart.plot(pruned_tree, box.palette="RdBu", shadow.col="gray",
           fallen.leaves=FALSE)


yhat=predict(pruned_tree,newdata=x.test,type='class')
table(y.test,yhat)
err<-1-mean(y.test==yhat)
err

