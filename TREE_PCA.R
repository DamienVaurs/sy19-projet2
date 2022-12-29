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

print(erreurOuter) # 0.1081081 0.1265509 0.1471322 0.1311881 0.1414392 0.1328321 0.1277641 0.1459854 0.1446078 0.1488834
print(mean(erreurOuter)) # 0.1354491
print(sd(erreurOuter)) # 0.01273021

# Erreur très élevée -> PCA ne change pas grand chose pour les arbres classiques 

rpart.plot(fit, box.palette="RdBu", shadow.col="gray",
           fallen.leaves=FALSE)

plotcp(fit)

