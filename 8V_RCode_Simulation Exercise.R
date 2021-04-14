##################################################################
#  8V: Simulation Exercise
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
#### SIMULATION FOR OVERLOOK HOTEL
##================================================================
## An empty vector to contain the output of interest
total_revenue <- c()
## Determine the value of the decision variable
reserved <- 100
## Determine the number of simulation replications to be made,
## use 10 000
nsim <- 10000
## Set the seed to zero
set.seed(0)
## Use a for loop to generate scenarios for each simulation
## replication
for(i in 1:nsim){
  ## Generate random variable from the probability distribution
  num_of_early <- rpois(n = 1, lambda = 150)
  num_of_late <-  rpois(n = 1, lambda = 140)
  ## Calculate the revenue for a given demand value
  ## You can use min(a,b) to take the minimum of values a and b
  if (num_of_early>250- reserved){
    total_revenue[i] <- (250-reserved)*170 + min(num_of_late,reserved)*200
  }else{
    total_revenue[i] <- num_of_early*170 + min(num_of_late, 250-num_of_early)*200
  }
}
total_revenue
sum(total_revenue)
