## ----Setup-------------------------------------------------
library(glmnet)

data("mtcars")

MSE <- function (obs, pred) {
  mse <- mean((obs - pred) ^ 2) 
  return (mse)
}

mtcars <- scale(as.matrix(mtcars))

ALPHA <- 1

reps <- 250


## ----Scenario 1--------------------------------------------
y <- mtcars[,  1]
x <- mtcars[, -1]

MSEs_1 <- rep(0, reps)

for (i in 1:reps) {
  cross_val <- cv.glmnet(x, y, nfolds = 5,
                         alpha = ALPHA, family = "gaussian")
  
  LAMBDA <- cross_val$lambda.1se
  lasso.mod <- glmnet(x, y, alpha = ALPHA, family = "gaussian",
                      lambda = LAMBDA)
  
  y.pred <- predict(lasso.mod, x)
  
  MSEs_1[i] <- MSE(y, y.pred)
}

MSE_1 <- mean(MSEs_1)

print(paste("MSE of Scenario 1:", round(MSE_1, 3)))


## ----Scenario 2--------------------------------------------

reps <- 250

MSEs_2 <- rep(0, reps)

for (i in 1:reps) {
  training_idcs <- sample(1:nrow(mtcars), floor(nrow(mtcars) * 0.9))
  
  training <- mtcars[training_idcs, ]
  test <- mtcars[-training_idcs, ]
  
  y.train <- training[,  1]
  x.train <- training[, -1]
  
  y.test <- test[,  1]
  x.test <- test[, -1]
  
  cross_val <- cv.glmnet(x.train, y.train, nfolds = 5,
                         alpha = ALPHA, family = "gaussian")
  
  LAMBDA <- cross_val$lambda.1se
  lasso.mod <- glmnet(x.train, y.train, alpha = ALPHA, family = "gaussian",
                      lambda = LAMBDA)
  
  y.pred <- predict(lasso.mod, x.test)
  
  MSEs_2[i] <- MSE(y.test, y.pred)
}

MSE_2 <- mean(MSEs_2)

print(paste("MSE of Scenario 2:", round(MSE_2, 3)))

