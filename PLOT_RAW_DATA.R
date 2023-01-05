library(ggplot2)

# Plot interval errors 
plot.cv.error <- function(data, x.title="x"){
  ic.error.bar <- function(x, lower, upper, length=0.1){ 
    arrows(x, upper, x, lower, angle=90, code=3, length=length, col='red')
  }
  stderr <- function(x) sd(x)/sqrt(length(x))
  # calculer les erreurs moyennes et l'erreur type (standard error)
  means.errs <- colMeans(data)
  std.errs <- apply(data, 2, stderr)
  # plotting  
  x.values <- 1:ncol(data)
  
  ggplot(data.frame(model=x.title, mean=as.vector(means.errs)),aes(x=model, y=mean))+ # New DF with the means
    geom_point(aes(x=model, y=mean))+
    geom_hline(yintercept=0.07, color="red")+
    ggtitle("Intervalle de confiance des erreurs - données brutes") +
    xlab("Modèles")+
    geom_errorbar(aes(ymin=mean - 1.6*std.errs, ymax= mean + 1.6*std.errs), width=.2,
                  position=position_dodge(.9)) +
    theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=60, hjust=1))
}

# PLOTS : 
err_bagging <- c(0.11013216, 0.10731707, 0.09523810, 0.04385965, 0.07031250, 0.08878505, 0.10087719, 0.07798165, 0.04761905, 0.06008584)
err_knn <- c(0.12807882, 0.10666667, 0.10185185, 0.09954751, 0.09787234, 0.08085106, 0.10833333, 0.09871245, 0.08219178, 0.08071749)
err_lda <- c(0.06437768, 0.07246377, 0.06557377, 0.07296137, 0.10714286, 0.06521739, 0.06603774, 0.09282700, 0.05504587, 0.11956522)
err_fda <- c(0.13145540, 0.06008584, 0.07818930, 0.07964602, 0.09361702, 0.09411765, 0.07582938, 0.04639175, 0.07211538, 0.04741379)
err_nb <- c(0.12000000, 0.13043478, 0.12500000, 0.12264151, 0.14847162, 0.11934156, 0.08796296, 0.14937759, 0.14601770, 0.11453744)
err_qda <- c(0.3427230, 0.3433476, 0.3045267, 0.3318584, 0.3404255, 0.3529412, 0.3601896, 0.3144330, 0.3557692, 0.3103448)
err_rf <- c(0.13615023, 0.08583691, 0.09876543, 0.08849558, 0.12340426, 0.09411765, 0.08056872, 0.08762887, 0.09615385, 0.09913793)
err_mda <- c(0.11737089, 0.05150215, 0.09876543, 0.08407080, 0.08936170, 0.09803922, 0.08530806, 0.07731959, 0.08173077, 0.06465517)
err_tree <- c(0.1100244, 0.1572482, 0.1369193, 0.1194030, 0.1820449, 0.1530864, 0.1410891, 0.1265509, 0.1485149, 0.1321696)
err_svm_vanilla <- c(0.0955667, 0.1198144, 0.13988165, 0.0888913, 0.0890077, 0.1133816, 0.0978746, 0.0791766, 0.0767365, 0.1009837)
err_svm_rbfdot <- c(0.08400244, 0.0512675, 0.0850123, 0.0700987, 0.0997015, 0.0499081, 0.0696199, 0.0399567, 0.0993349, 0.075)
err_svm_laplace <- c(0.0771725, 0.0498601, 0.1094556, 0.0691252, 0.0881743, 0.0723391, 0.0499671, 0.0508187, 0.0699714, 0.0714286)


err_combine <- array(c(err_bagging,
                       err_knn,
                       err_lda,
                       err_fda,
                       err_nb,
                       err_qda,
                       err_rf,
                       err_mda,
                       err_tree,
                       err_svm_vanilla,
                       err_svm_rbfdot,
                       err_svm_laplace
                       ), dim=c(10,12))

err_combine

labels <- c("Bagging", "KNN", "LDA", "FDA", "Naive Bayes", "QDA", "Random Forest", "MDA", "Decision tree", "SVM Linear", "SVM Gaussian", "SVM Laplace")


plot.cv.error(err_combine, x.title=labels)


?plot()
