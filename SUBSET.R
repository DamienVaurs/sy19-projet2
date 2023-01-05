library(leaps)
library(MASS)
library(naivebayes)
library(mda)
library(glmnet)
library(klaR)

set.seed(1998)
x <- read.table("phoneme_train.txt")
y <- as.factor(x$y)

n <- nrow(x)
p <- ncol(x) - 1

x <- cbind(as.data.frame(scale(x[,1:p])), y)

KfoldsOuter <- 10 #nb folds CV externe
KfoldsInner <- 5 #nb folds CV interne

erreurOuter <- rep(0, KfoldsOuter)
kOuter <- rep(0, KfoldsOuter)
foldsOuter = sample(1:KfoldsOuter, n, replace=TRUE) 

nb.pred <- rep(0, KfoldsOuter)
y <- rep(TRUE,p)
predicteurs <- matrix(nrow=KfoldsOuter,ncol=p+1)


for (i in 1:KfoldsOuter){ #CV externe : pour RDA (ou autre)
  print(i)
  x.train <- x[foldsOuter != i,]
  x.test <- x[foldsOuter == i,]
  
  foldsInner = sample(1:KfoldsInner, nrow(x.train), replace=TRUE)
  erreur.inner <- rep(0, KfoldsInner)
  
  predicteurs.inner <- matrix(nrow=KfoldsInner,ncol=p+1)
  nb.pred.inner <- rep(0,KfoldsInner)
  
  
  
  for (j in 1:KfoldsInner) { # CV interne : pour nb.pred optimal
    
    x.train.inner <- x.train[foldsInner != j,]
    x.test.inner <- x.train[foldsInner == j,]
    
    fit.inner <- summary(regsubsets(y~.,data=x.train.inner,method='forward',nvmax=p))
    nb.pred.inner[j] <- which.min(fit.inner$bic) #which.max(fit.inner$adjr2) 
    
    predicteursTotInner <- cbind(as.data.frame(fit.inner$which[,1:p+1]),y)
    predicteurs.inner[j,] <- predicteursTotInner[nb.pred.inner[j],]==TRUE
    
    xbest.train.inner <- x.train.inner[,predicteurs.inner[j,]]
    xbest.test.inner <- x.test.inner[,predicteurs.inner[j,]]
    
    fit.inner <- lda(y ~ ., data=xbest.train.inner)
    pred.inner <- predict(fit.inner, newdata=xbest.test.inner[,1:ncol(xbest.train.inner)-1])$class
    
    erreur.inner[j] <- 1 - mean(pred.inner == xbest.test.inner$y)
  }
  
  
  nb.pred[i] <- nb.pred.inner[which.min(erreur.inner)]
  predicteurs[i,] <- predicteurs.inner[which.min(erreur.inner),]
  
  xbest.train <- x.train[,predicteurs[i,]]
  xbest.test <- x.test[,predicteurs[i,]]
  
  fit <- lda(y ~ ., data=xbest.train)
  
  pred <- predict(fit, newdata=xbest.test[,1:ncol(xbest.train)-1])$class
  
  erreurOuter[i]<- 1 - mean(pred==xbest.test$y)
  print(erreurOuter[i])
}

print(erreurOuter) # 0.10798122 0.06866953 0.06584362 0.07079646 0.10638298 0.08627451 0.06635071 0.06701031 0.07211538 0.04741379
print(mean(erreurOuter)) # 0.07588385
print(sd(erreurOuter)) # 0.01897807
print(nb.pred)
print(mean(nb.pred))

# On va partir sur un nombre de prédicteurs moyen optimal de 34

