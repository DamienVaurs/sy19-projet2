library(MASS)

set.seed(1998)
x <- read.table("phoneme_train.txt")
y <- as.factor(x$y)

n <- nrow(x)
p <- ncol(x) - 1

x <- cbind(as.data.frame(scale(x[,1:p])), y)

# Hypothèse du modèle ici : distribution gaussienne

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

print(erreurOuter) # 0.3427230 0.3433476 0.3045267 0.3318584 0.3404255 0.3529412 0.3601896 0.3144330 0.3557692 0.3103448
print(mean(erreurOuter)) # 0.3356559
print(sd(erreurOuter)) # 0.01975289

## QDA : à proscrire, ici l'erreur moyenne est trop importante 
# Ici, QDA est un modèle trop général
