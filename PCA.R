library(FactoMineR)
library(factoextra)

set.seed(1998)
x <- read.table("phoneme_train.txt")
y <- as.factor(x$y)

n <- nrow(x)
p <- ncol(x) - 1

x <- cbind(as.data.frame(scale(x[,1:p])), y)

res.pca <- PCA(x, ncp=p, quali.sup = p+1, graph = FALSE)
print(res.pca)

# L'extraction des composantes principales détermine la quantité de variance expliquée par chaque
# composante principale
fviz_eig(res.pca, main="Pourcentage de variance expliquée par les composantes principales", barfill="#ff4305", barcolor="#ff4305", ncp = 15, addlabels = TRUE, ylim = c(0, 60))
