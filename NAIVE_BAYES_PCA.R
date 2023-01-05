library(naivebayes)

set.seed(1998)
x <- read.table("phoneme_train.txt")
y <- as.factor(x$y)

n <- nrow(x)
p <- ncol(x) - 1

pca <- princomp(x[,1:p])
x <- cbind(as.data.frame(pca$scores[,1:20]),y)
p <- ncol(x) - 1

KfoldsOuter <- 10 #nb folds CV externe

erreurOuter <- rep(0, KfoldsOuter)
kOuter <- rep(0, KfoldsOuter)
foldsOuter = sample(1:KfoldsOuter, n, replace=TRUE) 

for (i in 1:KfoldsOuter){
  
  x.train <- x[foldsOuter != i, ]
  x.test <- x[foldsOuter == i, 1:p]
  y.test <- x[foldsOuter == i, p+1]
  
  fit <- naive_bayes(y~.,data = x.train)
  
  pred <- predict(fit,newdata = x.test)
  
  erreurOuter[i] <- 1 - mean(pred==y.test)
}

print(erreurOuter) # 0.09859155 0.08583691 0.07407407 0.06637168 0.12765957 0.08627451 0.08530806 0.07216495 0.09134615 0.07327586 
print(mean(erreurOuter)) # 0.08609033
print(sd(erreurOuter)) # 0.01765081

# Meilleures performances ici car on a réduit, grâce à la PCA, le nombre de paramètres, et on sait que 
# Naive Bayes est plus performante avec un petit nombre de paramètres
