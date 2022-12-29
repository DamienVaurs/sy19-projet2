library(rpart)

set.seed(1998)
x <- read.table("phoneme_train.txt")
y <- as.factor(x$y)

n <- nrow(x)
p <- ncol(x) - 1

pca <- princomp(x[,1:p])
x <- cbind(as.data.frame(pca$scores[,1:20]),y)
p <- ncol(x) - 1

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

print(erreurOuter) # 0.1133005 0.1442786 0.1650000 0.1633663 0.1243781 0.1457286 0.1576355 0.1365854 0.1470588 0.1194030
print(mean(erreurOuter)) # 0.1416735
print(sd(erreurOuter)) # 0.01809771

