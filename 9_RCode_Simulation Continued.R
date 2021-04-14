##################################################################
#  9: Simulation Continued
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
#### SIMULATION FOR AIRLINE TICKETS
##================================================================
## An empty vector to contain the output of interest
total_profit <- c()
## Determine the values of decision variables
tickets_sold <- 106
## The number of simulation replications to be made is 10000
nsim <- 10000
## Set the seed to 0
set.seed(0)
## Use a for loop to generate scenarios for each simulation
## replication
for(i in 1:nsim){
  ## Generate random variable from the probability distribution
  ## that represents the number of passengers who show_up among
  ## all the tickets sold
  show_up <- rbinom(n=1, size =tickets_sold , prob= 0.94)
  ## Calculate the profit 
  if(show_up<=100){
    total_profit[i] <- tickets_sold*400
  }else{
    total_profit[i] <- tickets_sold*400 - (show_up-100)*800
  }
}

##================================================================
#### ANALYZE THE RESULTS FROM THE SIMULATION
##================================================================
## The probability of gaining more than 40 000 dollars 
mean(total_profit > 40000)

## Sample Mean
sample_mean <- mean(total_profit)
sample_mean
## Sample Standard Deviation
sample_sd <- sd(total_profit)
sample_sd
## Mean Standard Error
mse <- sample_sd/sqrt(nsim)
mse
## 95% Confidence Interval
lower_95ci <- sample_mean - 1.96*mse
upper_95ci <- sample_mean + 1.96*mse
lower_95ci
upper_95ci


## Estimating the Number of Trials Required to Reach the 
## Desired Precision
precision <- 5
std_dev <- sample_sd # standard deviation from an earlier simulation
num_of_trials <- (std_dev/precision * 2 * 1.96)^2 
num_of_trials

##================================================================
#### USER DEFINED R FUNCTIONS
##================================================================
## So far, we have been using functions that already exist. 
## Now, we will see how to write your own functions. 
## You use the same language you always use in R,
## in the same file as the rest of your code. 

## Example 1: 
# Let's define a function that takes the square of 
# the input
square_fun <- function(input) {
  output <- input*input
  return(output)
}

square_fun(input = 2)
square_fun(5)

## Example 2:
# You want to calculate the price you need to charge to 
# your customers based on hours of worked for the customer
# and your hourly rate
net_price_fun <- function(hours, price){
  net_price <- hours*price
  return(net_price)
}

net_price_fun(hours = 200, price = 40)
net_price_fun(200, 40)

## Exercise:
# Define a function called sum_func that calculates the 
# sum for three input variables. Then call the function 
# using arguments 5, 3, and 11.
sum_func <- function(x,y,z){
  sum_fun <- x+y+z
  return(sum_fun)
}

sum_func(5,3,11)

## Exercise:
# Define a function called total_price_func that takes three
# inputs, hours, price and tax_rate. Function calculates 
# the price you need to charge to your customers based on 
# hours of worked for the customer and your hourly rate, then
# adds the tax amount given as a input.
# Call the function using arguments hours is 200, price is 40,
# and tax rate is 6%.
total_price_func <- function(rate,price,tax){
  total_fun <- rate*price*(1+tax)
  return(total_fun)
}

total_price_func(200,40,.06)
## Example 3:
# Define a function called sumprod_func that calculates the 
# sum for three input variables and their product. 
# Then call the function using arguments 5, 3, and 11.
sumprod_func <- function(a,b,c) {
  output1 <- a + b + c
  output2 <- a * b * c
  return(c("Sum" = output1, "Product" = output2))
}
sumprod_func(5,3,11)

## Example 4: 
# For the airlines example, we can define a function called
# oversold_sim_func to 
# simulate the profit for different number of tickets sold
# and returns sample mean and lower and upper confidence 
# intervals.

oversold_sim_func <- function(tickets_sold){
  ## An empty vector to contain the output of interest
  total_profit <- c()
  ## Determine the number of simulation replications to be made
  nsim <- 10000
  ## Set the seed
  set.seed(0)
  ## Use a for loop to generate scenarios for each simulation
  ## replication
  for(i in 1:nsim){
    ## Generate random variable from the probability distribution
    show_up <- rbinom(n = 1, size = tickets_sold, prob = 0.94)
    ## Calculate the profit 
    if(show_up <= 100){
      total_profit[i] <- tickets_sold*400
    }else{
      total_profit[i] <- tickets_sold*400 - (show_up - 100)*800
    }
  }
  sample_mean <- mean(total_profit)
  sample_sd <- sd(total_profit)
  mse <- sample_sd/sqrt(nsim)
  lower_95ci <- sample_mean - 1.96*mse
  upper_95ci <- sample_mean + 1.96*mse
  
  return(c("Sample Mean" =  sample_mean, "Lower" = lower_95ci, "Upper" = upper_95ci))
}
oversold_sim_func(110)
oversold_sim_func(105)