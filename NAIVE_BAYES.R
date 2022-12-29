library(naivebayes)

set.seed(1998)
x <- read.table("phoneme_train.txt")
y <- as.factor(x$y)

n <- nrow(x)
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

print(erreurOuter) # 0.12000000 0.13043478 0.12500000 0.12264151 0.14847162 0.11934156 0.08796296 0.14937759 0.14601770 0.11453744
print(mean(erreurOuter)) # 0.1263785
print(sd(erreurOuter)) # 0.01868228

# Clairement moins bonnes performances que LDA ou MDA. Ceci est dû au fait que le classifieur naïf bayésien possède une 
# hypothèse supplémentaire : frontière quadratique entre les classes, et ne suppose pas, contrairement à LDA, l'égalité 
# des matrices de covariance. Ici, on a bcp de données (ce qui n'est pas idéal pour un classifieur de bayes naïf) + des
# frontières pas quadratiques -> ceci pourrait expliquer pourquoi naive bayes est un mauvais classifieur ici. 