##################################################################
#  7: Decision Trees Exercise Solutions
##################################################################
##
##================================================================
#### BUILDING A REGRESSION TREE EXERCISE 
##================================================================
## 1) Load the dataset
boston_data <- read.csv(file = 'BostonHousing.csv', header = T, as.is = T)

## 2) Partition the dataset as 60% training, set the seed to 1
train_size_bos <- 0.6 * nrow(boston_data)
set.seed(1)
train_index_bos <- sample(x = 1:nrow(boston_data), size = train_size_bos, replace = F)
boston_train <- boston_data[train_index_bos, ]
boston_valid <- boston_data[-train_index_bos, ]

## 3) Are there any predictors that needs to be removed from 
## the training set? Check the Environment and the documentation.
boston_train <- boston_train[ ,-14]
## 4) Fit a regression tree with cp = 0 (leave other arguments
##    same as the default)
library(rpart)
reg_tree_huge <- rpart(formula = MEDV ~ ., cp = 0,
                       data = boston_train, method = "anova")

## 5) Find the best cp value using cross validation, fit another 
##    regression tree using the best cp value
printcp(reg_tree_huge)
reg_tree <- rpart(formula = MEDV ~ ., cp = 0.01170223,
                  data = boston_train, method = "anova")
## 6) Visualize the tree from step 5
library(rpart.plot)
prp(reg_tree, type = 1, extra = 1)

## 7) Make predictions with the regression tree for the validation set
##    and calculate the RMSE value for the validation set
pred_tree_valid <- predict(object = reg_tree, newdata = boston_valid)
actual_valid <- boston_valid$MEDV
errors_tree_valid <- actual_valid - pred_tree_valid
RMSE_tree_valid <- sqrt(mean((errors_tree_valid)^2))
RMSE_tree_valid

## 8) Fit a linear regression model using forward stepwise
##    selection algorithm
linear_full <- lm(formula = MEDV ~  . , data = boston_train)
linear_mod <- step(object = linear_full, direction = 'forward', trace = 0)
summary(linear_mod)

## 9) Make predictions with the linear regression model for the validation set
##    and calculate the RMSE value for the validation set
pred_reg_valid <- predict(object = linear_mod, newdata = boston_valid)
actual_valid <- boston_valid$MEDV
errors_reg_valid <- actual_valid - pred_reg_valid
RMSE_reg_valid <- sqrt(mean((errors_reg_valid)^2))
RMSE_reg_valid 
