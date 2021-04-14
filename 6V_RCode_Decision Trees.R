##################################################################
#  6V: Decision Trees
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
#### LOAD PersonalLoan.csv TO R
## Dataset is on Canvas, do not forget to set your directory from 
## Session/Set Working Directory/Choose Directory
bank_data <- read.csv(file = 'PersonalLoan.csv', header = T, as.is = T)

##================================================================
#### DATA PARTITION
## I decided to divide the dataset as 80% training and 20% validation.
train_size <- 0.8 * nrow(bank_data)
set.seed(1)
train_index <- sample(x = 1:nrow(bank_data), size = train_size, replace = F)
train_set <- bank_data[train_index, ]
valid_set <- bank_data[-train_index, ]

##================================================================
#### INTRODUCTION: BUILDING AND VISUALIZING A CLASSIFICATION TREE
## To build a classification tree, we use the function called 
## rpart(). The first argument is formula (has the form 
## Target ~ Predictors), the second argument is data (the dataset
## you use to fit your model), and method = 'class'. Since our 
## target variable is categorical, we want a classification tree. 
## So, method = 'class'.

## Below we will generate a classification tree using 
## Income and CCAvg as predictors
install.packages('rpart')
library(rpart)
cl_tree_small <- rpart(formula = PersonalLoan ~ Income + CCAvg, 
                 data = train_set, method = "class")

## We use a function named prp(). The first argument is the 
## classification tree we received from the rpart() function.
## Arguments type and extra are used to change the appearance 
## of your tree. I like type = 1 and extra = 1. But you can 
## explore other tree shapes.
install.packages('rpart.plot')
library(rpart.plot)
prp(cl_tree_small, type = 1, extra = 1)

## Here are some other tree examples:
prp(cl_tree_small, type = 3, extra = 1)
prp(cl_tree_small, type = 0, extra = 2)
## For more information, type ??prp, click 'rpart.plot::prp',
## and look at the section titled Arguments.

##================================================================
#### BUILDING A CLASSIFICATION TREE
## Since decision trees incorporate variable selection into their
## decision making by design, you can include all predictors
## and let the tree to decide which predictors are important.
## However, you still need to be careful! If a variable is not 
## available when you are trying to predict the target variable
## (for instance, the final value of a home and the tax of the home),
## you should exclude that predictor from the tree. Otherwise, your
## tree might be useless.

## Here is how to include ALL columns except the target variables
## as a predictor. You use a dot (.) after ~ to denote that you want
## to use all variables as predictors.

## Since Education is categorical, I use as.factor function to change  
## its type.
train_set$Education <- as.factor(train_set$Education)
valid_set$Education <- as.factor(valid_set$Education)
library(rpart)
cl_tree <- rpart(formula = PersonalLoan ~ ., 
                 data = train_set, method = "class")

#### Generate the tree diagram
library(rpart.plot)
prp(cl_tree, type = 1, extra = 1)

##================================================================
#### MAKING CLASSIFICATIONS USING A CLASSIFICATION TREE
## For prediction, we use the function predict(). 
## There are three arguments. The first two are similar to what we 
## learned for regression. The third argument is always
## type = "prob" to get the probabilities from a decision 
## tree.
pred_loan_tree_prob <- predict(object = cl_tree, newdata = valid_set, 
                          type = "prob")

## Next, we turn probabilities into classifications
cutoff <- 0.5
pred_loan_tree <- ifelse(pred_loan_tree_prob[ ,2]>cutoff, yes = 1, no = 0)
##================================================================
#### EVALUATING CLASSIFICATION PERFORMANCE
## Since this is a classification problem, we will use confusion 
## matrix, accuracy, sensitivity, specificity and lift chart to evaluate 
## the classification performance.
library(caret)
actual_valid <- valid_set$PersonalLoan
confusionMatrix(as.factor(pred_loan_tree), 
                as.factor(actual_valid), positive = "1")

library(caret)
lift_tree <- lift(relevel(as.factor(valid_set$PersonalLoan), ref = "1") 
              ~ pred_loan_tree_prob[ ,2], data = valid_set)
xyplot(lift_tree, plot = "gain")

##================================================================
#### BUILDING A REGRESSION TREE 
## Load the dataset
car_data <- read.csv(file = 'ToyotaCorolla.csv', header = T, as.is = T)
train_size_car <- 0.6 * nrow(car_data)
set.seed(1)
train_index_car <- sample(x = 1:nrow(car_data), size = train_size_car, replace = F)
train_set_car <- car_data[train_index_car, ]
valid_set_car <- car_data[-train_index_car, ]

## Fit the regression tree
train_set_car <- train_set_car[ ,-1]
library(rpart)
reg_tree <- rpart(formula = Price ~ ., data = train_set_car, method = "anova")

## Visualization of the tree
library(rpart.plot)
prp(reg_tree, type = 1, extra = 1)

##================================================================
#### MAKING PREDICTIONS FROM A REGRESSION TREE
pred_price_tree <- predict(object = reg_tree, newdata = valid_set_car)

##================================================================
#### EVALUATING PREDICTION PERFORMANCE
actual_valid <- valid_set_car$Price
## Now, we can calculate the prediction errors.
errors_tree_valid <- actual_valid - pred_price_tree
## Then, calculate the ME and RMSE
ME_tree_valid <- mean(errors_tree_valid)
RMSE_tree_valid <- sqrt(mean((errors_tree_valid)^2))
c(ME_tree_valid, RMSE_tree_valid) #print them together

