library(mda)
library(mclust)

set.seed(1998)
x <- read.table("phoneme_train.txt")
y <- as.factor(x$y)

n <- nrow(x)
p <- ncol(x) - 1

x <- cbind(as.data.frame(scale(x[,1:p])), y)

# Ici on a une extension du modèle LDA : le modèle à mélange gaussien. 
# on fait varier le nombre de composants présents dans chaque mélange, et ainsi
# obtenir des régions de décision non-linéaires et complexes 
# On continue ici à supposer l'égalité des covariances

KfoldsOuter <- 10 #nb folds CV externe

erreurOuter <- rep(0, KfoldsOuter)
kOuter <- rep(0, KfoldsOuter)
foldsOuter = sample(1:KfoldsOuter, n, replace=TRUE) 

# Using mda package:
for (i in 1:KfoldsOuter){
  print(i)
  x.train <- x[foldsOuter != i, ]
  x.test <- x[foldsOuter == i, 1:p]
  y.test <- x[foldsOuter == i, p+1]
  
  fit <- mda(y ~ ., data=x.train)
  pred <- predict(fit, newdata=x.test)
  
  erreurOuter[i]<- 1 - mean(pred==y.test)
  print(erreurOuter[i])
  
  plot(x.test, y.test, pch=".", xlab = "x1", ylab="x2")
  
}

print(erreurOuter) # 0.11737089 0.05150215 0.09876543 0.08407080 0.08936170 0.09803922 0.08530806 0.07731959 0.08173077 0.06465517
print(mean(erreurOuter)) # 0.08481238
print(sd(erreurOuter)) # 0.01835668



