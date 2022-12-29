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
    ggtitle("Intervalle de confiance des erreurs avec sélection de sous-ensembles") +
    xlab("Modèles")+
    geom_errorbar(aes(ymin=mean - 1.6*std.errs, ymax= mean + 1.6*std.errs), width=.2,
                  position=position_dodge(.9)) +
    theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=60, hjust=1))
}

# PLOTS : 
err_bagging3 <- c(0.13145540, 0.06866953, 0.05761317, 0.07522124, 0.12765957, 0.09019608, 0.08530806, 0.06185567, 0.08653846, 0.05603448) #OK
err_knn3 <- c(0.0799173, 0.0795881, 0.0681533, 0.0988137, 0.1009975, 0.0761352, 0.0666815, 0.0614724, 0.0798935, 0.0677813) # OK
err_lda3 <- c(0.10328638, 0.06008584, 0.06995885, 0.07522124, 0.11063830, 0.07843137, 0.06161137, 0.06701031, 0.07692308, 0.05603448) # OK
err_fda3 <- c(0.10328638, 0.06008584, 0.06172840, 0.07079646, 0.11914894, 0.08235294, 0.06161137, 0.06185567, 0.08173077, 0.04741379) # OK
err_nb3 <- c(0.11737089, 0.09012876, 0.09876543, 0.09292035, 0.11489362, 0.12156863, 0.13270142, 0.07731959, 0.08653846, 0.06465517) # OK
err_qda3 <- c(0.08920188, 0.07296137, 0.09053498, 0.09292035, 0.13617021, 0.07843137, 0.09478673, 0.09278351, 0.11057692, 0.06896552) # OK
err_rf3 <- c(0.04225352, 0.05437768, 0.06172840, 0.08292035, 0.05957447, 0.07843137, 0.09004739, 0.07731959, 0.10576923, 0.08620690) # OK
err_mda3 <- c(0.11267606, 0.04721030, 0.09053498, 0.07522124, 0.10638298, 0.08235294, 0.08530806, 0.07216495, 0.06730769, 0.06896552) # OK
err_tree3 <- c(0.1198771, 0.1036973, 0.0970823, 0.0712871, 0.0814392, 0.0953383, 0.1274201, 0.1154501, 0.0976471, 0.1064020) # OK
err_svm_vanilla3 <- c(0.0397785, 0.0488610, 0.0799011, 0.0816387, 0.0915338, 0.0728993, 0.0566712, 0.0858712, 0.0814001, 0.0912452) # OK
err_svm_rbfdot3 <- c(0.0480934, 0.0567740, 0.0698134, 0.0899001, 0.0913751, 0.0537120, 0.0651756, 0.0749821, 0.0788014, 0.0595910) # OK
err_svm_laplace3 <- c(0.0791004, 0.0601554, 0.0559974, 0.0899154, 0.0521243, 0.0600972, 0.0428173, 0.0738541, 0.0818711, 0.0799846) # OK 

err_combine <- array(c(err_bagging3,
                       err_knn3,
                       err_lda3,
                       err_fda3,
                       err_nb3,
                       err_qda3,
                       err_rf3,
                       err_mda3,
                       err_tree3,
                       err_svm_vanilla3,
                       err_svm_rbfdot3,
                       err_svm_laplace3
                       ), dim=c(10,12))

err_combine

labels <- c("Bagging", "KNN", "LDA", "FDA", "Naive Bayes", "QDA", "Random Forest", "MDA", "Decision tree", "SVM Linear", "SVM Gaussian", "SVM Laplace")


plot.cv.error(err_combine, x.title=labels)
