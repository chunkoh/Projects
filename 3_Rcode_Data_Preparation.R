##################################################################
#  3: Data Preparation
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
#### LOAD THE WEST_ROXBURY DATASET
## Click: Session/Set Working Directory/Choose Directory and 
## choosing the folder directory that .csv file is in.
home_data <- read.csv(file = "K353_West_Roxbury.csv", 
                      header = T, as.is = T)

##================================================================
#### DETECTING MISSING VALUES
## summary() function tells you which variables have a missing 
## value in them
summary(home_data)

## is.na() returns a logical vector that tells you whether 
## each element in a vector is NA or not.
na_example <- c(1, 2, 4, NA, 5)
is.na(na_example)

## How do we obtain the count for the number of missing values? 
sum(is.na(na_example))

## R treats logical as numeric when it is in numeric functions. So,
## each TRUE = 1, each FALSE = 0, and sum gives you the
## total number of missing values in a column.

## Which variables have "NA" values? 
## which() function returns the indices of TRUEs in a logical vector.
which(is.na(na_example))

#### Exercise #####
# - Use these functions above to count how many records are missing
#   for the HALF.BATH variable and identify which observations 
#   have missing values? 
sum(is.na(home_data$HALF.BATH))
which(is.na(home_data$HALF.BATH))

##================================================================
#### DEALING WITH MISSING VALUES
##
## Option 1: Fill in value based on prior knowledge 
## e.g. replacing the missing values for HALF.BATH to 0s
index_NA <- which(is.na(home_data$HALF.BATH)) #indices with missing values
home_data$HALF.BATH[index_NA] <- 0 #replace
sum(is.na(home_data$HALF.BATH)) #NAs are gone

## Option 2: Replacing the value of an observation with mean
## e.g. replacing the missing values for LIVING.AREA with mean
index_NA <- which(is.na(home_data$LIVING.AREA))
mean_liv_area <- mean(home_data$LIVING.AREA, na.rm = TRUE)
## Recall: na.rm= TRUE compute option allows the operation 
## to be performed on all non-mising values
home_data$LIVING.AREA[index_NA] <- mean_liv_area

## Option 3: Removing observations with missing values 
## e.g. delete the second observation from dataset
dim(home_data)
index_to_delete <- which(is.na(home_data$TAX))
home_data <- home_data[-index_to_delete, ] 
dim(home_data) # observation deleted

##================================================================
#### DETECTING OUTLIERS
## Couple useful techniques to detect outliers:
## 1) Generate the box plot, look for distant points
boxplot(home_data$FLOORS)
## 2) Generate scatter plots with other columns, look
##    for distant points
plot(home_data$FLOORS, home_data$LOT.SQFT)
## 3) Look for maximum and minimums, do they make sense?
max(home_data$FLOORS)
min(home_data$FLOORS)


##================================================================
#### DEALING WITH OUTLIERS
## If outlier implies an actual data quality issue, the process
## is similar to the process for missing values (see above.)
##
## First, find the exact index of the outlier
index_outlier <- which(home_data$FLOORS == 10)
index_outlier #69th observation

#### Relational operators in R
## > : Greater than
## >=: Greater than or equal to 
## < : Less than
## <=: Less than or equal to
## ==: Exactly equal to
## !=: Not equal to
####

#### Exercise ####
# - Find the observations that has more than 2 floors?
which(home_data$FLOORS >2)
# - Find the observations that has a lot square ft higher 
#   higher than 40000.
which(home_data$LOT.SQFT >40000)

## Option 1: Deleting an observation from a data frame: 
## Negative indexing
## To delete the observation
home_data <- home_data[-index_outlier, ] 
dim(home_data)

## Option 2: Replacing the extreme values by less extreme ones
## e.g. median
val_med <- median(home_data$FLOORS, na.rm = TRUE)
home_data$FLOORS[index_outlier] <-  val_med

##================================================================
#### DATA TRANSFORMATION
## LINEAR TRANSFORMATION
## In R, if an arithmetic operation is between a vector and 
## a single number, operation is performed between the number 
## and each observation of the vector, respectively.
##
## Let's say we want to have a variable TOTAL.VALUE.USD that is
## in USD instead of in thousands of USD
home_data$TOTAL.VALUE.USD  <- home_data$TOTAL.VALUE*1000
head(home_data)

## TO FIX SKEWNESS
## Recap - Issues detected in the distribution plots
hist(home_data$TOTAL.VALUE)
## Does it have a left skew or a right skew?
rIGHT SKEW
## Creating a new column with transformed TOTAL.VALUE in the dataframe
home_data$LOG.TOTAL.VALUE <- log(home_data$TOTAL.VALUE+1)

hist(home_data$LOG.TOTAL.VALUE)
boxplot(home_data$LOG.TOTAL.VALUE)

#### Exercise ####
## - Can you create a new variable SQRT.TOTAL.VALUE that takes the
##   square root of the TOTAL.VALUE variable? Compare the histograms 
##   for TOTAL.VALUE, SQRT.TOTAL.VALUE and LOG.TOTAL.VALUE.
##   What do you observe? Which transformation makes more sense?
##   Hint: Use sqrt() instead of log().
home_data$sqrt.total.value <- sqrt(home_data$TOTAL.VALUE+1)
hist(home_data$sqrt.total.value)
##================================================================
#### CREATING NEW VARIABLES FROM THE VARIABLES IN THE DATASET  
## Let's say you want to create a new variable in home_data 
## that holds the value per lot sqft of an observation. 
## In other words, you want to divide TOTAL.VALUE by LOT.SQFT.

## If arithmetic operation is between two vectors of the same size,
## operation is performed between all the pairs of entries.
## Example:
# Here are production amounts and cost per unit.
# What is the total production cost for each product?
quantity <- c(1000, 1500, 1000, 500)
unit_cost <- c(5, 6, 10, 1)
total_cost <- quantity*unit_cost
total_cost

#### Exercise ####
## - How can we create a new column of VALUE.PER.SQFT?
valuepersqft <- home_data$TOTAL.VALUE/home_data$LOT.SQFT
home_data$valuepersqft <- home_data$

##================================================================
#### ACCESING OBSERVATIONS THAT SATISFY CERTAIN CONDITIONS
## Above, we learned how to find the indices of the observations 
## that satify certain conditions.
index_value800 <- which(home_data$TOTAL.VALUE > 800)
index_value800 

#### Relational operators in R
## >   Greater than
## >= Greater than or equal to
## <   Less than
## <= Less than or equal to
## == Exactly equal to
## !=  Not equal to

## And logical operators in R
## & And
## | Or
## ! Not
####

## Example:
index_value800_more5 <- which(home_data$TOTAL.VALUE > 800 & home_data$ROOMS > 5)
index_value800_more5 

#### Exercises ####
# - Find the observations that has a fireplace (FIREPLACE equals 1)

# - Find the indices of the observations whose total value is larger 
#   than 800 OR smaller than 100


# - Find the observations that has a fireplace AND has a total
#   value higher than 800 and save the indices of these 
#   observations to a R-variable named index_fire800


# - Average the TAX values of each observation that has a fireplace
#   AND has a total value higher than 800 and save this 
#   number to a R-variable named tax_fire800



