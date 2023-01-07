rm(list = ls())

data <- read.table('robotics_train.txt')

index <- sample(nrow(data), 0.8*nrow(data))
train <- data[index,]
test <- data[-index,]

# model SVR
library(caret)

c_vals <- c(0.1,0.25,0.5, 1, 10)
sigma_vals <- c(0.01, 0.1, 1, 10, 100)

# expand.grid create a grid of all the possible combinations of C and sigma
tune_grid <- expand.grid(C = c_vals, sigma = sigma_vals)

set.seed(5)

ctrl <- trainControl(method = "cv", number = 10)

svr.model <- train(y~., data=train, method="svmRadial", 
                   trcotrol=ctrl, tuneGrid = tune_grid)

svr.model.pred <- predict(svr.model, test)

error <- svr.model.pred - test[["y"]]

MSE <- mean(error^2)

print(MSE)

# Optimal values of hyperparameters C and sigma
best_params <- svr.model$bestTune

best_c <- best_params$C
#  10
best_sigma <- best_params$sigma
# 0.1

