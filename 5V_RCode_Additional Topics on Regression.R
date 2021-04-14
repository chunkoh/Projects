##################################################################
#  5V: Additional Topics on Regression
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
#### INTERACTION TERMS: EXAMPLE WITH LOGISTIC REGRESSION
##================================================================
#### LOAD PersonalLoan.csv TO R AND PARTITION THE DATASET
bank_data <- read.csv(file = 'PersonalLoan.csv', header = T, as.is = T)
bank_train_size <- 0.6 * nrow(bank_data)
set.seed(500)
bank_train_index <- sample(x = 1:nrow(bank_data), size = bank_train_size)
bank_train <- bank_data[bank_train_index, ]
bank_valid <- bank_data[-bank_train_index, ]

#### MODEL WITHOUT INTERACTION TERMS
## Logistic Model 0: Fit a model with two predictors
logistic_m0<- glm(formula = PersonalLoan ~ Family + Income
                  , data = bank_train, family = "binomial")
## Performance metrics
m0_prob <- predict(object = logistic_m0, newdata = bank_valid, 
                   type = "response")
library(caret)
lift0 <- lift(relevel(as.factor(bank_valid$PersonalLoan), ref = "1") 
              ~ m0_prob, data = bank_valid)
xyplot(lift0, plot = "gain")
cutoff <- 0.5
pred_m0 <- ifelse(m0_prob>cutoff,yes = 1, no = 0)
actual_class_valid <- bank_valid$PersonalLoan
confusionMatrix(as.factor(pred_m0), 
                as.factor(actual_class_valid), positive = "1")

#### ADDING INTERACTION TERMS
## Plot loan application decision for two variables
plot(bank_data$Family, bank_data$Income, 
     col = ifelse(bank_data$PersonalLoan == 1, yes = "red", no = "blue"),
     pch  = ifelse(bank_data$PersonalLoan == 1, yes = '+', no = '*'))

## Fit a model with interaction terms: use I() function
## to let R know that you are doing math.
logistic_m1<- glm(formula = PersonalLoan ~ Family + Income + I(Family*Income)
               , data = bank_train, family = "binomial")
m1_prob <- predict(object = logistic_m1, newdata = bank_valid, 
                   type = "response")

library(caret)
lift1 <- lift(relevel(as.factor(bank_valid$PersonalLoan), ref = "1") 
              ~ m1_prob, data = bank_valid)
xyplot(lift1, plot = "gain")

cutoff <- 0.5
pred_m1 <- ifelse(m1_prob>cutoff,yes = 1, no = 0)
actual_class_valid <- bank_valid$PersonalLoan
confusionMatrix(as.factor(pred_m1), 
                as.factor(actual_class_valid), positive = "1")

##================================================================
#### HIGHER ORDER TERMS: EXAMPLE WITH LINEAR REGRESSION
##================================================================
#### LOAD BostonHousing.csv TO R AND PARTITION THE DATASET
boston_data <- read.csv(file = 'BostonHousing.csv', header = T, as.is = T)
boston_train_size <- 0.6 * nrow(boston_data)
set.seed(500)
boston_train_index <- sample(x = 1:nrow(boston_data), size = boston_train_size)
boston_train <- boston_data[boston_train_index, ]
boston_valid <- boston_data[-boston_train_index, ]

## Plot loan application decision for two variables
plot(boston_data$LSTAT, boston_data$MEDV)

#### MODEL WITHOUT HIGHER ORDER TERMS
linear_m0 <- lm(formula = MEDV ~ LSTAT
                , data = boston_train)
pred_m0_valid <- predict(object = linear_m0, newdata = boston_valid)
actual_valid <- boston_valid$MEDV
errors_m0_valid <- actual_valid - pred_m0_valid
RMSE_m0_valid <- sqrt(mean((errors_m0_valid)^2))
RMSE_m0_valid 

#### ADDING HIGHER ORDER TERMS
## Fit a model with higher order terms: use I() function
## to let R know that you are doing math.
linear_m1 <- lm(formula = MEDV ~ LSTAT + I(LSTAT*LSTAT)
                , data = boston_train)
pred_m1_valid <- predict(object = linear_m1, newdata = boston_valid)
actual_valid <- boston_valid$MEDV
errors_m1_valid <- actual_valid - pred_m1_valid
RMSE_m1_valid <- sqrt(mean((errors_m1_valid)^2))
RMSE_m1_valid 

##================================================================
#### LOGARITHMIC TRANSFORMATION: EXAMPLE WITH LINEAR REGRESSION
##================================================================
## Plot target variable (MEDV) against LSTAT
plot(boston_data$LSTAT, boston_data$MEDV)

## Now plot target variable (MEDV) against log of LSTAT
plot(log(boston_data$LSTAT), boston_data$MEDV)

#### LOGARITHMIC TRANSFORMATION
## Fit a model with tranformed variable: use I() function
## to let R know that you are doing math.
linear_m2 <- lm(formula = MEDV ~ I(log(LSTAT)), data = boston_train)
pred_m2_valid <- predict(object = linear_m2, newdata = boston_valid)
errors_m2_valid <- actual_valid - pred_m2_valid
RMSE_m2_valid <- sqrt(mean((errors_m2_valid)^2))
RMSE_m2_valid

##================================================================
## WHAT ABOUT LINEAR TRANSFORMATIONS?
##================================================================
summary(boston_data)
## Let's fit a model with LSTAT transformed linearly
linear_m3 <- lm(formula = MEDV ~ I(LSTAT/100), data = boston_train)
pred_m3_valid <- predict(object = linear_m3, newdata = boston_valid)
errors_m3_valid <- actual_valid - pred_m3_valid
RMSE_m3_valid <- sqrt(mean((errors_m3_valid)^2))
RMSE_m3_valid 

## Compare it to the RMSE from model linear_m0
RMSE_m0_valid

## Look at the summary for both models
summary(linear_m0)
summary(linear_m3)

##================================================================
## STEPWISE SUBSET SELECTION
##================================================================
#### EXAMPLE WITH LINEAR REGRESSION
## Automatically selecting the best set of predictors to include
## First, generate a model with all predictors you want to consider.
## You can list all predictors by name OR you can use '.' (which 
## means include all variables in this dataset as predictors) 
## Remember to exclude the predictors you do not want from the 
## training set.
## Recall: Why do we exclude CATMEDV?
linear_full <- lm(formula = MEDV ~  . , data = boston_train[ , -14])

## Next, we use step() function to implement stepwise subset 
## selection.
## object: your model with all predictors
## direction: 'forward', 'backward' or 'both'
## trace = 0: to avoid function from printing stuff on the console
linear_m4 <- step(object = linear_full, direction = 'both', trace = 0)

## Use summary() or coef() functions to get the details of the final model
summary(linear_m4)

## Performance of the model
pred_m4_valid <- predict(object = linear_m4, newdata = boston_valid)
errors_m4_valid <- actual_valid - pred_m4_valid
RMSE_m4_valid <- sqrt(mean((errors_m4_valid)^2))
RMSE_m4_valid 

#### EXAMPLE WITH LOGISTIC REGRESSION
## Same application with a logistic regression model
logistic_full <- glm(formula = PersonalLoan ~  . , data = bank_train, family = 'binomial')
logistic_m2 <- step(object = logistic_full, direction = 'both', trace = 0)

summary(logistic_m2)

## Performance metrics
m2_prob <- predict(object = logistic_m2, newdata = bank_valid, 
                   type = "response")
library(caret)
lift2 <- lift(relevel(as.factor(bank_valid$PersonalLoan), ref = "1") 
              ~ m0_prob, data = bank_valid)
xyplot(lift2, plot = "gain")
cutoff <- 0.5
pred_m2 <- ifelse(m2_prob>cutoff,yes = 1, no = 0)
actual_class_valid <- bank_valid$PersonalLoan
confusionMatrix(as.factor(pred_m2), 
                as.factor(actual_class_valid), positive = "1")

##================================================================
#### INTERPRETING GOODNESS OF FIT
#### LINEAR REGRESSION: Use summary() function
summary(linear_m0)

#### LOGISTIC REGRESSION:
## Fit your logistic model with your predictors
logistic_model<- glm(formula = PersonalLoan ~ Family + Income
                  , data = bank_train, family = "binomial")
## Fit a naive model without any predictors
naivemod <- glm(formula = PersonalLoan ~ 1, data = bank_train,
                family = "binomial")
## Test the difference between the two models
anova(naivemod, logistic_model, test = 'Chisq')

