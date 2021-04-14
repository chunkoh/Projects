##################################################################
#  5: Logistic Regression
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

##================================================================
#### 1: Determine the target variable
##================================================================
## Target variable: PersonalLoan
corr(bank_data)
##================================================================
#### 2: Choose the predictors to include in your model
##================================================================
## Let's explore the dataset.
#Age, Income, Family, Experience
##================================================================
#### 3: Partition your dataset into training and validation sets
##================================================================
## I decided to divide the dataset as 60% training and 40% validation.
trainingsize <- .6*nrow(bank_data)
set.seed(1)
train_index <- sample(x=1:nrow(bank_data),size = trainingsize)
trainset <- bank_data[train_index,]
validset <- bank_data[-train_index,]
##================================================================
#### 4: Fit the coefficient values using training set
##================================================================
Model1 <- glm(formula = PersonalLoan ~ Age + Income + Family + Experience + Education, data = trainset, family = "binomial")

Model2 <- glm(formula = PersonalLoan ~ Age + Income + Family + Experience + as.factor(Education), data = trainset, family = "binomial")
summary(Model2)
##================================================================
#### 5: Classify the observations in the validation set using 
#### cut off of 0.5
##================================================================
pred_loan_prob <- predict(object = Model2 , newdata = validset, 
                          type = "response")
cutoff <- 0.5
pred_loan <- ifelse(pred_loan_prob>cutoff, yes = 1, no = 0)
##================================================================
#### 6: Evaluate the clasification performance of your model 
#### with the validation set assuming that  detecting customers 
#### who respond to the offer (PersonalLoan = 1) is more important
##================================================================
install.packages("e1071")
library(caret)

actual_valid <- validset$PersonalLoan
confusionMatrix(as.factor(pred_loan), 
                as.factor(actual_valid), positive = "1")
##================================================================
#### 7: Evaluate the clasification performance of your model 
#### with the validation set when cutoff is changed to 0.3 assuming 
#### that  detecting customers who respond to the offer
#### (PersonalLoan = 1) is more important
##================================================================
cutoffnew <- 0.3
pred_loan_new <- ifelse(pred_loan_prob>cutoffnew, yes = 1 , no = 0)

confusionMatrix(as.factor(pred_loan_new), 
                as.factor(actual_valid), positive = "1")
##================================================================
#### 8: Compare the accuracy and sensitivty values of the two 
#### cutoff values. If detecting customers who respond to the 
#### offer (PersonalLoan = 1) is more important, which cutoff 
#### value makes more sense to use?
##================================================================


##================================================================
#### Lift Charts
##================================================================
## Specify the important class in the target variable
## compute lift as the ability of the model to identify the important
## class, as proportion of observations selected
##  - In as.factor(), you give your target variable
##  - ref equals to the important class
##  - After ~ you give the probabilities estimated by your model
library(caret)
lift1 <- lift(relevel(as.factor(validset$PersonalLoan), ref = "1") 
              ~ pred_loan_prob, data = validset)
xyplot(lift1, plot = "gain")
