##################################################################
#  10: Optimization
##################################################################
##================================================================
## These R-notes contain explanatory text, R code examples, 
## and exercises. 
## You can use this file to take your own notes. Follow 
## along, write down solutions to exercises and take notes
## using comments as you need.
##================================================================

##================================================================
#### OPTIMIZATION IN R
## There is a function in R called optim() that can implement  
## various algorithms to solve your optimization problem. However,
## we need to turn our problem into an R code that is suitable for
## this functions.

##================================================================
#### EXAMPLE: HOTEL PRICING
## 1) Create a function in R that calculates the objective function
## Rules to keep in mind:
##   - Decision variables must be the first input
##   - If you have multiple decision variables, all need to be 
##     part of the same vector

hotel_revenue_fun <- function(dec_vars) {
  # Define decision varaibles as each element of the dec_vars vector
  S <- dec_vars[1]
  G <- dec_vars[2]
  P <- dec_vars[3]
  # Calculate the objective function value and return it
 revenue <- (625 -4.41*S)*S + (300-2.04*G)G + (100-.35*P)P
 return(revenue)
}

## 2) Test the function you created
test <- c(100, 100, 130)
hotel_revenue_fun(test) #should return 35085

## 3) Incorporate constraints into the R function as penalty
## Guidelines:
##    - If your problem is a minimization problem, any 
##      infeasbile solution should return Inf
##    - If your problem is a maximization problem, any 
##      infeasbile solution should return -Inf
hotel_revenue_fun <- function(dec_vars) {
  # Define decision varaibles as each element of the dec_vars vector
  S <- dec_vars[1]
  G <- dec_vars[2]
  P <- dec_vars[3]
  # Calculate the objective function value and return it
  revenue <- (625 -4.41*S)*S + (300-2.04*G)G + (100-.35*P)P
  if(S<70 | S>90 | G<90 | G>110 | P<120 | P>150 | 1025-4.41*S - 2.04*G - 0.35*P >450){
    return(-Inf)
  }else{
    return(revenue)
  }
}

## 4) Test the function to make sure feasible/infeasible solutions are correct
test <- c(100, 100, 130)
hotel_revenue_fun(test) #should return -Inf
test2 <- c(80, 100, 130)
hotel_revenue_fun(test2) #should return 38461

## 5) Select one feasible solution to be used as a starting point for
##    optim() function
feasible_sol <- c(80, 100, 130)

## 6) Apply optim to your problem, save its solution to a variable
##    Arguments:
##        - par: the feasible solution to be used as a starting point
##        - fn: function to be minimized
##        - control = list(fnscale = -1): added when maximizing
opt_sol <- optim(par = feasible_sol
                 , fn = hotel_revenue_fun
                 , control = list(fnscale = -1))

## 7) From the solution variable returned by optim(), you can access
## both the parameters that achieve the solution and the 
## resulting objective function value
opt_sol$par # decision variable values that achieve solution
opt_sol$value # objective function value at the solution
