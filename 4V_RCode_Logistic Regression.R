##################################################################
#  4V: Logistic Regression
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
#### EXPLORING THE PersonalLoan.csv DATASET: USING SCATTER PLOTS
#### FOR CLASSIFICATION

#### SOMETHING NEW: ifelse() function 
## Arguements: ifelse(test, yes, no); 
## test: a condition involving a vector
## yes: value to return for true elements of test.
## no: value to return for false elements of test.

## Example:
vec1 <- c(15,20,25,30,35)
ifelse(vec1 >= 25, yes = 1, no = 0) #returns 1 or zero
ifelse(vec1 >= 25, yes = 'yes', no = 'no') #returns 'yes' or 'no'

#### LABELING A SCATTER PLOT
## A regular scatter plot
plot(bank_data$Income, bank_data$CCAvg)

## Scatter plot labeled for Personal Loan data
plot(bank_data$Income, bank_data$CCAvg, 
     col = ifelse(bank_data$PersonalLoan == 1, yes = "red", no = "blue"),
     pch  = ifelse(bank_data$PersonalLoan == 1, yes = '+', no = '*'))

## Tables with two categoricals
table(bank_data$PersonalLoan, bank_data$Education)
table(bank_data$PersonalLoan, bank_data$Family)
table(bank_data$PersonalLoan, bank_data$Online)

## Side by side box plots
boxplot(bank_data$Mortgage ~ bank_data$PersonalLoan)

##================================================================
#### DATA PARTITION
## Divide the dataset as 60% training and 40% validation.
train_size <- 0.6 * nrow(bank_data)
set.seed(1)
train_index <- sample(x = 1:nrow(bank_data), size = train_size)
train_set <- bank_data[train_index, ]
valid_set <- bank_data[-train_index, ]

##================================================================
#### WHAT IS LOGISTIC REGRESSION?
##================================================================
#### FITTING A LOGISTIC REGRESSION MODEL
## The function glm() calculates the fitted logistic regression 
## model by maximum likelihood estimation .
## The first two arguments are similar to the arguments in lm(),
## but there is a third argument that you HAVE to include.
## This argument will always be family = "binomial" for a 
## logistic regression model.
fit_reg <- glm(formula = PersonalLoan ~ Income + CCAvg, 
               data = train_set, family = "binomial")

## The logistic regression model is 
## log(odds of Personal Loan = 1) = b0+b1*Income+b2*CCAvg, and 
## following code gives the best b0, b1 and b2 values.
coef(fit_reg)

## For coefficients and significance levels
summary(fit_reg)

## To convert log of odds to probabilities
model_coef <- coef(fit_reg)
logofodds <- sum(c(1, 112, 1.4) * model_coef)
odds <- exp(logofodds)
prob <- odds/(1+odds)
prob

##================================================================
#### MAKING CLASSIFICATIONS USING THE FITTED MODEL
## For prediction, we use the function predict() again. 
## There are three arguments. The first two are similar to what we 
## learned for linear regression. The third argument is always
## type = "response" when you are doing classification.
pred_loan_prob <- predict(object = fit_reg, newdata = valid_set, 
                          type = "response")

## Next, we turn probabilities into classifications
cutoff <- 0.5
pred_loan <- ifelse(pred_loan_prob>cutoff, yes = 1, no = 0)

##================================================================
#### EVALUATING CLASSIFICATION PERFORMANCE
## Function confusionMatrix() from a package called caret calculates 
## confusion matrix and multiple classification performance measures.
## There are three arguments. First one is classifications  from your 
## model and the second argument is the actual classes. The third
## one denotes to important class. This is used to calculate 
## specificity and sensitivity values. In our example, detecting
## customers who respond to the offer (PersonalLoan = 1) is more
## important, so positive = "1".
install.packages("")
library(caret)

actual_valid <- valid_set$PersonalLoan
confusionMatrix(as.factor(pred_loan), 
                as.factor(actual_valid), positive = "1")

### FAQ ABOUT LOADING PACKAGES
## - If you see this error: "Error: `data` and `reference` should 
## be factors with the same levels." Then, make sure that you have 
## as.factor() for both predicted vector and actual vector.
## - If you see an error similar to: "package e1071 is required", 
## try running the following (without comments)
#install.packages('e1071')
#install.packages('caret')
#library(caret)
## - If you see an error similar to: "package XXX is required", 
## try running the following (without comments) with replacing 
## XXX with the package name in the error 
#install.packages('XXX')
#install.packages('caret')
#library(caret)