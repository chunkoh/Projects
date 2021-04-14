##################################################################
#  9V: Estimation
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
#### LOAD donations.csv TO R
donations_data <- read.csv(file = 'donations.csv', header = T, as.is = T)

##================================================================
#### FITTING A PROBABILITY DISTRIBUTION TO TargetD (CONTINUOUS)
##================================================================
## Let's focus on the people who actually donated
donated_id <- which(donations_data$TargetB == 1)
donated_data <- donations_data[donated_id, ]

## Let's look at the histogram of average donation amount
hist(donated_data$TargetD)

#### SELECT CANDIDATE DISTRIBUTIONS
## To fit a distribution, you can use an existing package in R
## You need to run install.packages only once for one computer
install.packages('fitdistrplus')
## You need to specify the library everytime you open RStudio
library(fitdistrplus)

## This function from the package fitdistrplus gives you a graph 
## that helps you to fit data
descdist(donated_data$TargetD, discrete=FALSE)

#### ESTIMATE THE PARAMETERS OF THE CHOSEN DISTRIBUTIONS
## Function fitdist from the package fitdistrplus gives you the 
## parameter values that fit your data. 
fit_D_g  <- fitdist(donated_data$TargetD, "gamma")
fit_D_ln  <- fitdist(donated_data$TargetD, "lnorm")
## You can see the fitted parameter values by using
fit_D_g$estimate
fit_D_ln$estimate

#### EVALUATE THE QUALITY OF FIT
## Function denscomp from the package fitdistrplus overlaps the
## histogram with the fitted distributions

## To check the fit of the distribution
denscomp(fit_D_ln)
denscomp(fit_D_g)
## Or side by side
denscomp(list(fit_D_ln, fit_D_g))

#### GENERATING RANDOM VARIABLES FOR YOUR SIMULATION
## Generate random variables based on the fitted distribution
fit_D_g$estimate
sim_D <- rgamma(n = 1, shape = 2.5586266, rate = 0.1637738)
sim_D

##================================================================
#### EXERCISE: FITTING A PROBABILITY DISTRIBUTION TO GiftTimeLast
##================================================================
## Let's look at the dataset to see if GiftTimeLast is discrete
## You can see if a variable is discrete from the documentation 
## and from how R sees this variable. If the variable is an integer
## it is likely to be discrete (but not always! Make sure to check
## the documentation.)
str(donations_data)

#### 1) SELECT CANDIDATE DISTRIBUTIONS
library(fitdistrplus)
descdist(donations_data$GiftTimeLast, discrete=TRUE)
#### 2) ESTIMATE THE PARAMETERS OF THE CANDIDATE DISTRIBUTIONS
fit_G_norm  <- fitdist(donations_data$GiftTimeLast, "norm")
fit_G_nb  <- fitdist(donations_data$GiftTimeLast, "nbinom")
fit_G_norm$estimate
fit_G_nb$estimate
#### 3) EVALUATE THE QUALITY OF FIT
## Which distribution did you select?
denscomp(list(fit_G_norm, fit_G_nb))

#### 4) GENERATING RANDOM VARIABLES FOR YOUR SIMULATION
## Generate one random value from the distribution you selected
rnbinom(n = 1, size = 1.045646e+06, mu = 1.800277e+01)

##================================================================
#### SIMULATING THE DONATION AMOUNT FROM THE NEXT DONOR WHEN THERE
#### IS NO DATA ABOUT THE NEW DONOR
##================================================================
donations_vec <- c()
nsim <- 1000
set.seed(0)
for (i in 1:nsim){
  sim_D <- rgamma(n = 1, shape = 2.5586266, rate = 0.1637738)
  donations_vec[i] <- sim_D
}

## Probability of receiving more than $10
mean(donations_vec > 10)

##================================================================
#### SIMULATING THE DONATION AMOUNT FROM THE NEXT DONOR WHEN THERE
#### IS SOME AMOUNT OF DATA ABOUT THE NEW DONOR
#### SIMULATING WHETHER THESE DONORS WILL ACTUALLY DONATE
##================================================================
new_donations_data <- read.csv(file = 'new_donors.csv', header = T, as.is = T)
summary(new_donations_data)

##================================================================
#### Use the data from other donors to predict whether these donors
## would donate

## Partition the data, 60% training
set.seed(500)
train_size <- 0.6 * nrow(donations_data)
train_index <- sample(x = 1:nrow(donations_data), size = train_size)
train_set <- donations_data[train_index, ]
valid_set <- donations_data[-train_index, ]

## Fit a logistic regression model using TargetB as the target variable
## and GiftAvgLast and GiftAvgAll as predictors
model_log <- glm(formula = TargetB ~  GiftAvgLast + GiftTimeLast,
                 data = train_set, family = 'binomial')
summary(model_log)

## Use the model's predicted probabilities in the model
prob_unk <- predict(object = model_log, newdata = new_donations_data, 
                    type = 'response')

## Simulate the donation amount for donor number 1
donor_id <- 1
donations_vec2 <- c()
set.seed(0)
nsim <- 1000
for (i in 1:nsim){
  donate_or_not <- rbinom(n = 1, size = 1, prob = prob_unk[donor_id])
  sim_D <- rgamma(n = 1, shape = 2.5586266, rate = 0.1637738)
  donations_vec2[i]<- sim_D * donate_or_not
}

## probability of receiving more than $10
mean(donations_vec2 > 10)

##================================================================
#### SIMULATING THE DONATION AMOUNT FROM THE NEXT DONOR WHEN THERE
#### IS SOME AMOUNT OF DATA ABOUT THE NEW DONOR
#### PREDICTING THE NEXT DONATION AMOUNT INSTEAD OF USING THE AVERAGE
##================================================================

##================================================================
#### Use the data from other donors to predict how much these new
#### donors would donate if they do donate

## Partition the data, 60% training
set.seed(500)
train_size_don <- 0.6 * nrow(donated_data)
train_index_don <- sample(x = 1:nrow(donated_data), size = train_size_don)
train_set_don <- donated_data[train_index_don, ]
valid_set_don <- donated_data[-train_index_don, ]

## Fit a linear regression model using TargetD as the target variable
## and GiftAvgAll, GiftAvgLast, GiftCntAll and GiftTimeFirst as predictors
model_lin <- lm(formula = TargetD ~ GiftAvgAll + GiftAvgLast + GiftCntAll,
             data = train_set_don)
summary(model_lin)

## Calculate the prediction errors from the validation data
avgdon_valid <- predict(object = model_lin, newdata = valid_set_don)
errors <- valid_set_don$TargetD - avgdon_valid
hist(errors, main = "Prediction Errors" )
boxplot(errors, main= "Box plot of Prediction Errors")

## Fitting a distribution to prediction errors
library(fitdistrplus)
descdist(errors, discrete = FALSE)

fit_error_norm <- fitdist(errors, "norm")
fit_error_norm$estimate

pred_error <- rnorm(n = 1, mean = -0.2095047, sd = 8.8407143)

## Use the model to predict the donation amount of new donors and 
## simulate the outcomes
pred_don <- predict(object = model_lin, newdata = new_donations_data)
pred_don

donor_id <- 2
donations_vec3 <- c()
set.seed(0)
nsim <- 1000
for (i in 1:nsim){
  donate_or_not <- rbinom(n = 1, size = 1, prob = prob_unk[donor_id])
  pred_error <- rnorm(n = 1, mean = -0.2095047, sd = 8.8407143)
  sim_D <- pred_don[donor_id] + pred_error
  donations_vec3[i]<- sim_D * donate_or_not
}

## probability of receiving more than $10
mean(donations_vec3 > 10)

##================================================================
#### USING A FUNCTION TO RUN THE ANALYSIS FOR DIFFERENT DONORS
##================================================================
## A function that calculates the probability of donating more than
## 10 dollars using simulation for a given donor_id
donation_func <- function(donor_id){
  donations_vec3 <- c()
  nsim <- 1000
  set.seed(0)
  for (i in 1:nsim){
    donate_or_not <- rbinom(n = 1, size = 1, prob = prob_unk[donor_id])
    pred_error <- rnorm(n = 1, mean = -0.2095047, sd = 8.8407143)
    sim_D <- pred_don[donor_id] + pred_error
    donations_vec3[i]<- sim_D * donate_or_not
  }
  
  ## probability of receiving more than $10
  prob <- mean(donations_vec3 > 10)
  return(prob)
}

## Run the function for different donor ids
donation_func(1)
donation_func(2)
donation_func(3)
donation_func(4)

## Using a for loop to run the function for all donor ids
prob_vec <- c()
my_val <- 1:10
for(i in 1:length(my_val)){
  donor_id <- my_val[i]
  prob_vec[i] <- donation_func(donor_id)
}
prob_vec

