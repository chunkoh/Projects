##################################################################
#  4: Linear Regression
##################################################################
##================================================================
## These R-notes contain explanatory text, R code examples, 
## and exercises. 
## You can use this file to take your own notes. Follow 
## along, write down solutions to exercises and take notes
## using comments as you need.
##================================================================
##
##================================================================
#### LOAD ToyotaCorolla.csv TO R
##================================================================
## Dataset is on Canvas, do not forget to set your directory from 
## Session/Set Working Directory/Choose Directory
car_data <- read.csv(file = 'ToyotaCorolla.csv', header = T, as.is = T)

##================================================================
#### STEP 1: Determine the target variable
##================================================================
## Target variable: Price

##================================================================
#### STEP 2: Choose the predictors to include in your model
##================================================================
## Let's explore the dataset.
cor(car_data[,-c(1:5)])

##================================================================
#### STEP 3: Partition your dataset into training and validation sets
##================================================================
## Divide the dataset as 60% training and 40% validation. Do not
## forget to use set.seed(500)
set.seed(500)

train_size <- 0.6 * nrow(car_data)
train_index <- sample(x=1:nrow(car_data),size= train_size)
train_set <- car_data[train_index,]
valid_set <- car_data[-train_index,]

##================================================================
#### STEP 4: Fit the coefficient values using training set
##================================================================
## Use the function lm() to fit the coefficients.
model1 <- lm(formula = Price ~ Age + KM + CC, data=train_set)
coef(model1)


## What are the fitted coefficient values for your model?

## FYI: if we don't want to use the scientific notations (with 'e's), 
## use the following and run coef() function again. 
options(scipen=999)
summary(model1)
##================================================================
#### STEP 5: Predict the target variable for the observations 
#### in the validation set
##================================================================
## Predictions for the validation set using predict() function
pred_valid <- predict(object=model1, newdata= valid_set)

##================================================================
#### STEP 6: Evaluate the predictive performance of your model 
#### by checking the predictive performance with the validation set
##================================================================
## Get the actual prices in the validation set.
actual_validprice <- valid_set$Price
## Calculate the prediction errors.
error_validprice <- actual_validprice - pred_valid
## Calculate the ME and RMSE
ME_model1 <- mean(error_validprice)
RMSE_model1valid <- sqrt(mean((error_validprice)^2))
## Plots for prediction errors 

##================================================================
#### STEP 7: Evaluate the overfitting behavior by comparing the 
#### predictive performance with the training set against the
#### predictive performance with the validation set
##================================================================
## Predictions for the training set
pred_training <- predict(object=model1, newdata= train_set)
## Actual prices in the training set
actual_trainprice <- train_set$Price
## Prediction errors for the training set
error_trainprice <- actual_trainprice - pred_training
## Calculate RMSE for the training set
RMSE_modeltrain <- sqrt(mean((error_trainprice)^2))
## Compare against the RMSE for the validation set
