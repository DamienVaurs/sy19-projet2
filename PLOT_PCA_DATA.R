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
    ggtitle("Intervalle de confiance des erreurs avec les 15 premières composantes principales") +
    xlab("Modèles")+
    geom_errorbar(aes(ymin=mean - 1.6*std.errs, ymax= mean + 1.6*std.errs), width=.2,
                  position=position_dodge(.9)) +
    theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=60, hjust=1))
}

# PLOTS : 
err_bagging2 <- c(0.1408450, 0.0901287, 0.0864197, 0.0884955, 0.1276595, 0.0823529, 0.1042654, 0.0824742, 0.0865384, 0.1163791)
err_knn2 <- c(0.1079812, 0.0729613, 0.0864197, 0.0796460, 0.1148936, 0.0941175, 0.0805687, 0.0721649, 0.0865384, 0.0818965)
err_lda2 <- c(0.1032863, 0.0686695, 0.0617284, 0.0530973, 0.1063829, 0.0941176, 0.0853080, 0.0721645, 0.0673076, 0.0646551)
err_fda2 <- c(0.10328638, 0.06866953, 0.06172840, 0.05309735, 0.10638298, 0.09019608, 0.08530806, 0.07216495, 0.06730769, 0.06465517)
err_nb2 <- c(0.0985915, 0.0858369, 0.0740740, 0.0663716, 0.1276595, 0.0862745, 0.0853080, 0.0721649, 0.0913461, 0.0732758)
err_qda2 <- c(0.1173708, 0.0686695, 0.0740740, 0.0707966, 0.1191489, 0.0901960, 0.0995260, 0.0567010, 0.0721153, 0.0862069)
err_rf2 <- c(0.1361502, 0.0858369, 0.0987654, 0.0884955, 0.1234042, 0.0941175, 0.0805687, 0.0876288, 0.0961538, 0.0991373)
err_mda2 <- c(0.0985915, 0.0686695, 0.0617284, 0.0530973, 0.1021276, 0.0862745, 0.0616113, 0.0721645, 0.0673076, 0.0775862)
err_tree2 <- c(0.1081081, 0.1265509, 0.1471322, 0.1311881, 0.1414392, 0.1328321, 0.1277641, 0.1459854, 0.1446078, 0.1488834)
err_svm_vanilla2 <- c(0.0590061, 0.08136649, 0.0470113, 0.0766914, 0.0509778, 0.06816832, 0.0791002, 0.101147, 0.0801345, 0.100983)
err_svm_rbfdot2 <- c(0.0871744, 0.0499167, 0.0913771, 0.0591521, 0.0797015, 0.0899466, 0.0714725, 0.0410498, 0.0799147, 0.0915385)
err_svm_laplace2 <- c(0.0511683, 0.0799017, 0.0794776, 0.1009035, 0.0591534, 0.0777154, 0.0665433, 0.0534699, 0.0711736, 0.0888742)


err_combine <- array(c(err_bagging2,
                       err_knn2,
                       err_lda2,
                       err_fda2,
                       err_nb2,
                       err_qda2,
                       err_rf2,
                       err_mda2,
                       err_tree2,
                       err_svm_vanilla2,
                       err_svm_rbfdot2,
                       err_svm_laplace2
                       ), dim=c(10,12))

err_combine

labels <- c("Bagging", "KNN", "LDA", "FDA", "Naive Bayes", "QDA", "Random Forest", "MDA", "Decision tree", "SVM Linear", "SVM Gaussian", "SVM Laplace")


plot.cv.error(err_combine, x.title=labels)


?plot()
