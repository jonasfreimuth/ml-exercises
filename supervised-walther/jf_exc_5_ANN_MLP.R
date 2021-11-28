# This script has been adapted from the course script for Machine Learning
#   in Bioinformatics by Dirk Walther during SS 21
#   https://docs.google.com/document/d/1dKHGEJgKK4_MftD5NDOTPzDNdoMhvCDaRe_I0PPVbpA/edit#

library(keras)
library(tidyr)

d <- read.table("data/survey-2021-05.tab", header = TRUE) %>% 
  subset(select = c("gender", "cm", "kg", "ssize", "gp")) %>% 
  drop_na()



# shuffle data to destroy any effect of year data was taken
d <- d[sample(1:nrow(d)), ]


# Y-variable, binary output, M=1, F=0
Y <- ifelse(d$gender == "M", 1, 0)
# X variables
X <- as.matrix(subset(d, select = c("cm", "kg", "ssize", "gp")))
#dimnames(X)=NULL

Nobs <- nrow(d) # number of observations
Nvar <- 4       # number of variables

# generate membership vector for test or training data
set <- sample(2, Nobs, replace = TRUE, prob = c(0.7, 0.3))

trainingX <- X[set == 1, ]
trainingY <- Y[set == 1]
trainingN <- length(trainingY)

testX = X[set == 2, ]
testY = Y[set == 2]
testN = length(testY)

# normalize training data and apply mean and sd to test data as well
meanValues = apply(trainingX, 2, mean)

sdValues = apply(trainingX, 2, sd)

for (i in 1:Nvar) {
  
  trainingX[, i] = scale(trainingX[, i],
                         center = meanValues[i],
                         scale = sdValues[i])
  
  testX[, i] = scale(testX[, i],
                     center = meanValues[i],
                     scale = sdValues[i])
  
}

## define MultiLayerPerceptron = ANNinp
model = keras_model_sequential()

model %>%  
  layer_dense(units = 4,
              activation = 'sigmoid',
              input_shape = c(Nvar)) %>%
  layer_dense(units = 3, activation = 'sigmoid') %>%
  
  # layer_dropout(rate=.334) # add a “dropout rate” to output of layer before,
  # here appr. one  output value is set to zero
  
  # 'sigmoid' for binary, 'softmax' for multiclass,
  # see https://www.dlology.com/blog/how-to-choose-last-layer-activation-and-loss-function/
  # activation could also be ‘linear’, ‘relu’, etc.
  layer_dense(units = 1, activation = 'sigmoid')

# set different learning rate (lr) for sgd (stochastic gradient descent) 
#   or adam (adaptive moment estimation)
# sgd <- optimizer_sgd(lr = 0.01)
# adam<- optimizer_adam(lr=0.01) # default lr=0.001

model %>% compile(loss = 'binary_crossentropy',
                  # for binary outcome, 'categorical_crossentroy' for multiclass
                  optimizer = 'adam',
                  # or 'sgd' or other
                  metrics = c('accuracy'))

history <- model %>%
  fit(
    trainingX,
    trainingY,
    
    # One Epoch is when an ENTIRE dataset is passed forward and backward 
    #   through the neural network only ONCE
    epochs = 200,
    
    # minibatch, default =32, customary to pick power(2) series for better 
    # performance on GPUs
    batch_size = 32,
    
    # split into 80% training 20% validation, can also be preselected sets 
    #   'validation_data' The validation data is selected from the last samples
    #   in the x and y data provided, before shuffling.
    validation_split = 0.2 
    
  )

# apply to independent test dataset

model %>% evaluate(testX, testY)

prob <- model %>% predict(testX)                # probabilities
predClass <- model %>% predict_classes(testX)   # binary class calls

# confusion table
table(predicted = predClass, actual = testY)


# One hot encoding --------------------------------------------------------






