##################################################################
#  3V: Linear Regression
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
#### LOAD BostonHousing.csv TO R
## Dataset is on Canvas, do not forget to set your directory from 
## Session/Set Working Directory/Choose Directory
boston_data <- read.csv(file = 'BostonHousing.csv', header = T, as.is = T)

##================================================================
#### WHAT IS LINEAR REGRESSION?
##================================================================
##================================================================
#### FITTING A LINEAR REGRESSION MODEL: LSTAT AS PREDICTOR
## The linear model is MEDV = b0 + b1*LSTAT
## The function lm() calculates the fitted linear regression model
## by minimizing the mean squared error.
## The first argument of the function is formula in which you 
## specify the target variable and the predictor variable using
## the column names. 
## The second argument is called data and it is the name of the 
## dataframe that includes the dataset you want to use.
fit_sim_reg <- lm(formula = MEDV ~ LSTAT, data = boston_data)

## To see the fitted coefficient values, you use coef() function.

## The linear model is MEDV = b0 + b1*LSTAT, and following code 
## gives the best b0 and b1 values.
coef(fit_sim_reg)

##================================================================
#### FITTING A LINEAR REGRESSION MODEL: LSTAT, RM AND PTRATIO AS PREDICTORS
## The linear model is MEDV = b0 + b1*LSTAT + b2*RM + b3*PTRATIO
## If you decide to add multiple predictor variables, you still
## use the function lm(). You add all predictive variables
## you want to include to the formula by seperating them with
## plus signs as below:
fit_mul_reg <- lm(formula = MEDV ~ LSTAT + RM + PTRATIO, data = boston_data)

## The linear model is MEDV = b0 + b1*LSTAT + b2*RM + b3*PTRATIO, and 
## following code gives the best b0, b1, b2 and b3 values.
coef(fit_mul_reg)

##================================================================
#### EXAMPLE: BUILDING A LINEAR REGRESSION MODEL FOR PREDICTION
##================================================================
## We will continue using the BostonHousing.csv.

#### STEP 1: Determine the target variable
## Target variable: MEDV

#### STEP 2: Choose the predictors to include in your model
## Let's explore the dataset.
str(boston_data)

## I'll compute the correlation coefficients between MEDV 
## (target variable) and other numerical variables.
## I am excluding CHAS and CATMEDV
round(cor(boston_data[ , c(-4, -14)]),2)

## Side by side boxplots for CHAS
boxplot(boston_data$MEDV ~ boston_data$CHAS)
## Let's choose: LSTAT, RM, CHAS

#### STEP 3: Partition your dataset into training and validation sets
## I decided to divide the dataset as 60% training and 40% validation.
## First we calculate the size of the training set given the size 
## of the full dataset
train_size <- 0.6 * nrow(boston_data)

## Since we will partition our data randomly, use set.seed to ensure
## that you get the same training and validation datasets every time
set.seed(1)
## Using sample() function, we generate random integers, each 
## represent a row index. Selected rows will be used in the training
## set and the rest will be used in the validation set.
train_index <- sample(x = 1:nrow(boston_data), size = train_size)

## Finally, we partition our dataset into two based on the selected row 
## numbers
train_set <- boston_data[train_index, ]
valid_set <- boston_data[-train_index, ]

#### STEP 4: Fit the coefficient values using training set
## The linear model is MEDV = b0 + b1*LSTAT + b2*RM + b3*CHAS
m1_medv <- lm(formula = MEDV ~ LSTAT + RM + CHAS, data = train_set)
coef(m1_medv)

#### STEP 5: Predict the target variable for the observations 
#### in the validation set
## For prediction, we use a function called predict(). There are
## two arguments, first one is called object, it is the name of 
## the linear regression model you want to use for prediction.
## Second one is called newdata, it includes the dataset that
## includes the observations whose prices will be predicted.
pred_m1_valid <- predict(object = m1_medv, newdata = valid_set)

#### STEP 6: Evaluate the predictive performance of your model 
#### by checking the predictive performance with the validation set
## First, let get the actual MEDV values from the validation set.
actual_valid <- valid_set$MEDV
## Now, we can calculate the prediction errors.
errors_m1_valid <- actual_valid - pred_m1_valid
## Then, calculate the ME and RMSE
ME_m1_valid <- mean(errors_m1_valid)
RMSE_m1_valid <- sqrt(mean((errors_m1_valid)^2))
c(ME_m1_valid, RMSE_m1_valid) #print them together

## Generating plots of prediction errors
boxplot(errors_m1_valid, main= "Box plot of Prediction Errors")
hist(errors_m1_valid, main= "Histogram of Prediction Errors")

#### STEP 7: Evaluate the overfitting behavior by comparing the 
#### predictive performance with the training set against the
#### predictive performance with the validation set
## Now, we calculate the predictive performance for training set
## Predictions for the training set
pred_m1_train <- predict(object = m1_medv, newdata = train_set)
## Actual MEDV for the training set
actual_train <- train_set$MEDV
## Prediction errors for the training set
errors_m1_train <- actual_train - pred_m1_train
## Calculate the RMSE
RMSE_m1_train <- sqrt(mean((errors_m1_train)^2))
RMSE_m1_train

#if you want to get rid of the 'e' notation, use
options(scipen=999)

## We already calculated the predictive performance for 
## validation set
RMSE_m1_valid
