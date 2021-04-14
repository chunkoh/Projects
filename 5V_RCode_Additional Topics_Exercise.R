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
#### EXERCISES
##================================================================
## 1) Load ToyotaCorolla.csv to R partition the dataset as 60% training
## and set the seed to 500
toyota_data <- read.csv(file = 'ToyotaCorolla.csv', header = T, as.is = T)
trainset <- .6*nrow(toyota_data)
set.seed(500)
trainindex <- sample(x=1:nrow(toyota_data), size = trainset)
toyotatrain <- toyota_data[trainindex,]
toyotavalid <- toyota_data[-trainindex,]

## 2) Fit a baseline model with Age, KM and Mfr_Guarantee, and calculate
## RMSE for the validation set
model1 <- lm(formula = Price ~ Age + KM + Mfr_Guarantee, data = toyotatrain)
summary(model1)
predict1 <- predict(object = model1, newdata = toyotavalid)
actualvalid <- toyotavalid$Price
errorm1 <- actualvalid - predict1
RMSEm1 <- sqrt(mean(errorm1^2))
## 3) Generate a scatter plot of Age and Price. Then, generate a 
## scatter plot of log of Age and Price.
plot(toyota_data$Age,toyota_data$Price)
plot(log(toyota_data$Age),toyota_data$Price)
## 4) Fit a model with log of Age, KM and Mfr_Guarantee, and calculate
## RMSE for the validation set. Did performance of the model improve?
model2 <- lm(formula = Price ~ I(log(Age)) + KM + Mfr_Guarantee, data = toyotatrain)
summary(model2)
predict2 <- predict(object = model2, newdata = toyotavalid)
errorm2 <- actualvalid - predict2
RMSEm2 <- sqrt(mean(errorm2^2))
## 4) Now, try a model with higher order of Age In other words,
## your predictors will be Age, Age^2, KM,and Mfr_Guarantee. Calculate
## RMSE for the validation set. Did performance of the model improve?
model3 <- lm(formula = Price ~ Age + I(Age*Age)+ KM + Mfr_Guarantee, data = toyotatrain)
summary(model3)
predict3 <- predict(object = model3, newdata = toyotavalid)
actualvalid <- toyotavalid$Price
errorm3 <- actualvalid - predict3
RMSEm3 <- sqrt(mean(errorm3^2))

## 5) Try implementing the automated selection with forward selection 
## algorithm to find another 
## model. Calculate its RMSE for the validation set. Did performance
## of the model improve?
linear_full <- lm(formula = Price ~  . , data = toyotatrain[ , -1])
linear_m4 <- step(object = linear_full, direction = 'forward', trace = 0)
summary(linear_m4)
predict4 <- predict(object = linear_m4, newdata = toyotavalid)
errorm4 <- actualvalid - predict4
RMSEm4 <- sqrt(mean(errorm4^2))

## 6) Which model provides the lowest RMSE? Considering the 
## potential for overfitting, which model would you prefer?
