##################################################################
#  7: Decision Trees
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
##================================================================
## Dataset is on Canvas, do not forget to set your directory from 
## Session/Set Working Directory/Choose Directory
bank_data <- read.csv(file = 'PersonalLoan.csv', header = T, as.is = T)

## I decided to divide the dataset as 80% training and 20% validation.
## Set seed to 500.
train_size <- 0.8 * nrow(bank_data)
set.seed(500)
train_index <- sample(x = 1:nrow(bank_data), size = train_size, replace = F)
train_set <- bank_data[train_index, ]
valid_set <- bank_data[-train_index, ]

##================================================================
#### DECISION TREE EXERCISE
##================================================================
## Build a decision tree for Personal Loan target variable including
## all predictors in the dataset. Call your decision tree cl_tree,
## and generate the tree diagram.
train_set$Education <- as.factor(train_set$Education)
valid_set$Education <- as.factor(valid_set$Education)
library(rpart)
cl_tree <- rpart(formula = PersonalLoan ~ ., 
                 data = train_set, method = "class")

library(rpart.plot)
prp(cl_tree, type = 1, extra = 1)
##================================================================
#### DECISION TREES AND DECISION RULES
##================================================================
## A table of decision rules, first column is the probability
## of Personal Loan being equal to 1
library(rpart.plot)
rpart.rules(cl_tree)

##================================================================
#### MODIFYING DECISION TREE STOPPING RULES
##================================================================
## The argument minsplit determine the minimum number of observations
## that must exist in a node in order for a split to be attempted
## (minsplit = 20 if not specified)
cl_tree_mins <- rpart(formula = PersonalLoan ~ ., minsplit = 100,
                      data = train_set, method = "class")
prp(cl_tree_mins, type = 1, extra = 1)

## The argument minbucket determine the minimum number of observations 
## in any leaf node (minbucket = round(minsplit/3) if not specified)
cl_tree_minb <- rpart(formula = PersonalLoan ~ ., minbucket = 100,
                      data = train_set, method = "class")
prp(cl_tree_minb, type = 1, extra = 1)

## The argument maxdepth determine the maximum depth of any node 
## of the final tree, with the root node counted as depth 0
## (maxdepth = 30 if not specified)
cl_tree_maxd <- rpart(formula = PersonalLoan ~ ., maxdepth = 4,
                      data = train_set, method = "class")
prp(cl_tree_maxd, type = 1, extra = 1)

## The argument cp detemines the complexity parameter (CP),
## any split which does not improve the fit by CP amount
## will not be pursued (cp = 0.01 if not specified)
cl_tree_cp <- rpart(formula = PersonalLoan ~ ., cp = 0.0001,
                    data = train_set, method = "class")
prp(cl_tree_cp, type = 1, extra = 1)

##================================================================
#### FINDING THE BEST CP VALUE USING CROSS VALIDATION
##================================================================
## Fit a classification tree with cp as 0 and minsplit as 10 (if you
## visualize it, it will be huge!)
cl_tree_huge <- rpart(formula = PersonalLoan ~ ., cp = 0, minsplit =10,
                    data = train_set, method = "class")
prp(cl_tree_huge, type = 1, extra = 1)

## The function printcp() prints a table that shows how much 
## each split decreases the complexity parameter. 
printcp(cl_tree_huge)

## Best pruned tree
cl_tree_pruned <- rpart(formula = PersonalLoan ~ ., cp = 0.0075949,
                 data = train_set, method = "class")
prp(cl_tree_pruned, type = 1, extra = 1)

##================================================================
#### BUILDING A REGRESSION TREE EXERCISE 
##================================================================
## 1) Load the dataset
boston_data <- read.csv(file = 'BostonHousing.csv', header = T, as.is = T)

## 2) Partition the dataset as 60% training, set the seed to 1
train_size1 <- 0.6 * nrow(boston_data)
set.seed(1)
train_index1 <- sample(x = 1:nrow(boston_data), size = train_size1, replace = F)
train_set1 <- boston_data[train_index1, ]
valid_set1 <- boston_data[-train_index1, ]

## 3) Are there any predictors that needs to be removed from 
## the training set? Check the Environment and the documentation.
train_set1 <- train_set1[,-14]
library(rpart)
cl_tree1A <- rpart(formula = MEDV ~ ., 
                 data = train_set1, method = "anova")

library(rpart.plot)
prp(cl_tree1A, type = 1, extra = 1)
## 4) Fit a regression tree with cp = 0 (leave other arguments
##    same as the default)
cl_tree2A <- rpart(formula = MEDV ~ ., cp = 0.03098895, 
                   data = train_set1, method = "anova")
library(rpart.plot)
prp(cl_tree2A, type = 1, extra = 1)
printcp(cl_tree2A)
## 5) Find the best cp value using cross validation, fit another 
##    regression tree using the best cp value

## 6) Visualize the tree from step 5

## 7) Make predictions with the regression tree for the validation set
##    and calculate the RMSE value for the validation set

## 8) Fit a linear regression model using forward stepwise
##    selection algorithm

## 9) Make predictions with the linear regression model for the validation set
##    and calculate the RMSE value for the validation set

