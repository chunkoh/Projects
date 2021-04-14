##################################################################
#  8V: Simulation
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
#### RANDOM NUMBER GENERATORS
##================================================================
## When working with random numbers, do not forget to set your seed!

## BINOMIAL DISTRIBUTION: rbinom()
##  n: number of observations
##  size: number of trials
##  prob: probability of an event happening
set.seed(0)
rbinom(n = 1, size = 100, prob = 0.5)
## Example:Suppose in there are 280 participants registered for
##         an online seminar. Each participant has probability 
##         0.8 to attend. Then, the number of participants who
##         will attend can be represented with a binomial 
##         distribution and can be simulated with
set.seed(0)
rbinom(n = 1, size = 280, prob = 0.8)

## UNIFORM DISTRIBUTION: runif()
##  n: number of observations
##  min: minimum of the range
##  max: maximum of the range
set.seed(0)
runif(n = 1, min = 0, max = 1)
## Example:Slater customers are charged for the amount of 
##         salad they take. Data suggest that the amount 
##         of salad taken is uniformly distributed between 
##         5 ounces and 15 ounces. Then, the weight of salad 
##         of the next customer is uniformly distributed and 
##         can be simulated with
set.seed(0)
runif(n = 1, min = 5, max = 15)

## NORMAL DISTRIBUTION: rnorm()
##  n: number of observations
##  mean: mean 
##  sd: standard deviation
set.seed(0)
rnorm(n = 1, mean = 0, sd = 1)
## Example:Pep Zone sells auto parts and supplies including 
##         a popular multi-grade motor oil. Suppose the demand
##         during replenishment lead-time is normally 
##         distributed with a mean of 15 gallons and a standard
##         deviation of 2.5 gallons. Then, the demand for the 
##         next period is normally distributed and can be 
##         simulated with
set.seed(0)
rnorm(n = 1, mean = 15, sd = 2.5)

## POISSON DISTRIBUTION: rpois()
##  n: number of observations
##  lambda: the average number of times the event occurs over 
##          that time period
set.seed(0)
rpois(n = 1, lambda = 10)
## Example:Data from the maternity ward in a certain hospital
##         shows that there is a historical average of 4.5 
##         babies born in this hospital every day. Then, the 
##         number of babies born each day in the hospital can
##         be represented with a Poisson distribution and can 
##         be simulated with
set.seed(0)
rpois(n = 1, lambda = 4.5)

## GENERAL DISCRETE DISTRIBUTION: sample()
##  x: set of possible outcomes
##  size: number of observations 
##  prob: probabilities of each outcome
set.seed(0)
sample(x = 1:3, size = 1, replace = TRUE, prob = c(0.1, 0.5, 0.4))
## Example:Based on historical weather data, we know city X's
##         daily weather in the coming week can be: sunny, 
##         cloudy, rainy, or thunderstorm. The corresponding
##         probabilities are 0.35, 0.5, 0.1, 0.05. Then, 
##         tomorrow's weather in city X follows a discrete 
##         distribution and can be simulated with
set.seed(0)
sample(x = c('sunny', 'cloudy', 'rainy', 'thunderstorm'), 
       size = 1, replace = TRUE, prob = c(0.35, 0.5, 0.1, 0.05))

##================================================================
#### SIMULATION FOR ITECH COMPUTERS EXERCISE
##================================================================
## An empty vector to contain the output of interest
profit_results <- c()
## Determine the value of the decision variable
inventory <- 845000
## Determine the number of simulation replications to be made
nsim <- 10000
## Set the seed
set.seed(0)
## Use a for loop to generate scenarios for each simulation
## replication
for(i in 1:nsim){
  ## Generate random variable from the probability distribution
  demand <- runif(n = 1, min = 600000, max = 1300000)
  ## Calculate the profit for a given demand value
  if (demand > inventory){
    profit_results[i] <- inventory*800 - inventory*400
  }else{
    profit_results[i] <- demand*800 - inventory*400 + (inventory-demand)*200
  }
}

##================================================================
#### IF-ELSE STATEMENTS
##================================================================
## If-else statements can be very useful in R. Often, you want to  
## make choices and take action dependent on a certain value.
x <- 5
if(x < 3){
  y <- x
}else{
  y <- 3
}

## You can add if-else statements back to back
x <- 5
if(x < 3){
  y <- x
}else if(x < 10){
  y <- 3
}else{
  y <- 10
}

## And remember, you can use logical operators: &, | and !
x <- 5
z <- 10
if(x < 3 & z < 5){
  y <- x
}else{
  y <- 3
}

## EXERCISE:
## You want your code to calculate the price you need to charge
## to each customer based on your hours of worked for the customer,
## your hourly rate and the tax amount. 
## First, we define the values for hours worked, price per hour
## and client type.
hours <- 200
price <- 40
client_type <- 'foreign' #can be public, private or foreign.

## First, complete the code below to calculate the net price.
## If hours worked is above 100 hours, you give a 10% discount.
if(hours>100){
  # giving 10% discount if customer qualifies
  net_price <- hours*price * .9
}else{
  net_price <- hours*price
}
net_price

## Now we add the tax amount.
## Your tax rate depends on whether client 
## is public, private or foreign. The tax rate is 6% if customer
## is public, 12% if the customer is private, and 0% if the
## customer is foreign. Now add tax based on client_type 
## by completing the code below
if(client_type == "public"){
  final_price <- net_price * 1.06      
} else  if(client_type == "private"){
  final_price <- net_price * 1.12    
} else {
  final_price <- net_price  
}
final_price