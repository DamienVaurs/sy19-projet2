

# install.packages("visdat")
# install.packages("ggplot2")
library(visdat)
library(ggplot2)


data <- read.csv("communities_train.csv")

na_columns <- sapply(data, is.na)
na_counts <- colSums(na_columns)
selected_columns <- na_counts > 0
subset_data <- subset(data, select = selected_columns)

vis_miss(subset_data,warn_large_data = FALSE,sort_miss = TRUE) + theme(text = element_text(size=10),axis.text.x = element_text(angle=90,hjust=0))

# Missing values article --> https://journalofbigdata.springeropen.com/articles/10.1186/s40537-021-00516-9
# Function missingno

################### Données manquantes par régression linéaire #####################

non_numeric_cols <- which(sapply(data, function(x) !is.numeric(x)))
data_without_non_numeric <- subset(data, select = -non_numeric_cols)
cols_with_missing_values <- which(sapply(data_without_non_numeric, function(x) any(is.na(x))))

for (col in cols_with_missing_values) {
  not_missing <- !is.na(data_without_non_numeric[, col])
  x <- data_without_non_numeric[not_missing, -col]
  y <- data_without_non_numeric[not_missing, col]
  model <- lm(y ~ ., data = x)
}

missing <- is.na(data_without_non_numeric[, col])
z <- data_without_non_numeric[missing, -col]
options(warn=-1)
predictions <- predict(model, newdata = z)
data_without_non_numeric[missing, col] <- predictions


