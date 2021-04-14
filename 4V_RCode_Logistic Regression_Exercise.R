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
#### EXERCISE: BUILDING A LOGISTIC REGRESSION MODEL FOR CLASSIFICATION
##================================================================
## We will use the TitanicPassengers.csv
## Dataset is on Canvas, do not forget to set your directory from 
## Session/Set Working Directory/Choose Directory
titanic_data <- read.csv(file = 'TitanicPassengers.csv', header = T, as.is = T)
## Target variable: Survived
## Let's select Fare and Passenger.Class as predictors.

#### Q1: Partition your dataset into training and validation sets
#### as 60% training and 40% validation. Use argument 500 for 
#### set.seed
trainingset <- .6*nrow(titanic_data)
set.seed(500)
trainindex <- sample(x=1:nrow(titanic_data), size = trainingset)
train_set <- titanic_data[trainindex,]
valid_set <- titanic_data[-trainindex,]

#### Q2: Estimate the coefficient values using training set. What
## are the coefficient values? Which coefficients are significant?
## Save the coefficient values to an R-variable named model1_coef.
model1_coef <- glm(formula = Survived ~ Fare + Passenger.Class, data = train_set, family = 'binomial')
coef(model1_coef)
summary(model1_coef)
model1_coefT <- coef(model1_coef)
#### Q3: What is the log of odds of surviving, odd of surviving and 
#### probability of surviving according to the
#### model if a passenger paid a fare of 152 and was in Class 2?
logsofodds <- sum(c(1,152,1,0)*model1_coefT)
odds <- exp(logsofodds)
prob <- odds/(1+odds)
#### Q4: Calculate the probability of surviving for the passengers
#### in the validation set. 
pred_valid_set <- predict(object = model1_coef, newdata = valid_set , type = 'response')
#### Q5: Classify the observations in the validation set using
#### your model and cutoff of 0.5.
cutoff <- .5
pred_valid <- ifelse(pred_valid_set>.5, yes =1 , no= 0 )
#### Q6: Generate the confusion matrix for the validation set
#### assuming that detecting passengers who did not survive 
#### (Survived = 0) is more important.
#### What is the accuracy, specificity and sensitivity
#### of your model?
library(caret)
actual_valid <- valid_set$Survived
confusionMatrix(as.factor(pred_valid),as.factor(actual_valid), positive = '0')
#### Q7: Now classify the observations in the validation set using
#### your model and cutoff of 0.75.
cutoffnew <- .75
pred_valid_new <- ifelse(pred_valid_set>.75, yes= 1 , no = 0 )

confusionMatrix(as.factor(pred_valid_new),as.factor(actual_valid),positive = '0')
#### Q8: For the new classifications with the higher cutoff 
#### value, generate the confusion matrix for the validation set
#### assuming that detecting passengers who did not survive 
#### (Survived = 0) is more important.
#### What is the accuracy, specificity and sensitivity
#### of your model?
#increase cut off, sensitivity increase, specificity decrease

#### Q9: Using cutoff of 0.5, evaluate the overfitting behavior 
#### by comparing the classification performance with the training 
#### set against the classification performance with the validation
#### set. Remember, overfitting happens when your model classifies
#### your training set much better than your validation set.

