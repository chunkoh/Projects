##################################################################
#  8: Cluster Analysis
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
#### LOAD Utilities.csv TO R
##================================================================
## Dataset is on Canvas, do not forget to set your directory from 
## Session/Set Working Directory/Choose Directory
util_data <- read.csv(file = 'Utilities.csv', header = T, as.is = T)

##================================================================
#### K-MEANS CLUSTERING EXAMPLE WITH TWO VARIABLES
##================================================================
#### DATA PREPARATION:
## 1. Standardization and choosing attributes
## Create a new data frame with standardized values
util_data_std <- data.frame(sapply(util_data[, -1], scale))

## 2. APPLYING K-MEANS CLUSTERING ALGORITHM
## Here, we cluster using two variables: Sales and Fuel Cost. 
## First, I am visualizing the dataset with a scatter plot:
plot(util_data$Sales, util_data$FuelCost 
     , xlab = 'Sales'
     , ylab = 'FuelCost')

## Three clusters seems to be a good number for this problem.
## I use kmeans() function to determine the three clusters using
## the standardized Sales and Fuel Cost data. The first argument
## is the standardized dataset and the second argument is the number
## of clusters we want.
set.seed(0)
km_subset<- kmeans(util_data_std[, c('Sales', 'FuelCost')], centers = 3)

## 3. RESULTS: Now, km_subset holds all the information about our 
## clusters. You can obtain information about the clusters using 
## the following codes:
## $size: The number of observations in each cluster.
km_subset$size
## $centers: Returns the variable values for the cluster centroids.
km_subset$centers

## Plotting the clusters
plot(util_data$Sales, util_data$FuelCost 
     , xlab = 'Sales'
     , ylab = 'Fuel Cost'
     , col = km_subset$cluster)

## To obtain distance between clusters
dist(km_subset$centers)

##================================================================
#### EXERCISE
##================================================================
#### LOAD Customers.csv TO R
## Dataset is on Canvas, do not forget to set your directory from 
## Session/Set Working Directory/Choose Directory
cust_data <- read.csv(file = 'Customers.csv', header = T, as.is = T)

#### 1) Are there any missing values in the dataset?


#### 2) Generate the scatter plot of the customers in the cust_data 
## dataset between variables online expense and income.


#### 3) According to the plot you generated, how many clusters would
## you use for this dataset?

#### 4) Create new variables that holds standardized 
## income and online expense values.

#### 5) Using the standardized data you created and the number of 
## clusters you selected, generate the clusters using the 
## k-means algorithm and both income and online_expense columns.
## Set your seed to zero.

#### 6) How many observations are there in each cluster?

#### 7) What is the values for the cluster centroids?

#### 8) Generate a scatter plot in which each observation is labeled
## according to its cluster.



##================================================================
#### FINDING THE BEST K TO USE (USE OF FOR LOOPS)
##================================================================
## Imagine that you want to use all variables in the Utilities.csv.

## But how to choose the k value?

## We want the within cluster sum of squares for each cluster to be as
## small as possible.
## Therefore, we will calculate the average within cluster sum of 
## squares for different k values.

## Let's try different k values. The objective is to 
##  - run the kmeans() function for each k value from 1:10
##  - calculate the average within cluster sum of squuares for each 
set.seed(0)
km2 <- kmeans(util_data_std, centers = 2)
mean(km2$withinss)

set.seed(0)
km3 <- kmeans(util_data_std, centers = 3)
mean(km3$withinss)

set.seed(0)
km4 <- kmeans(util_data_std, centers = 4)
mean(km4$withinss)

set.seed(0)
km5 <- kmeans(util_data_std, centers = 5)
mean(km5$withinss)

## What if there are many k-values we want to test? A way to 
## automate it! 
#### FOR LOOPS ####
## EXAMPLE 1:
## Storing the squares of each number in a vector
##  my_results: Start with empty vector to contain your results
##  my_values: Define the vector that contains the numbers
##  i is your counter

my_results <- c()
my_values <- c(3,5,6,7,8)
for(i in 1:length(my_values)) {
  the_value <- my_values[i]^2
  my_results[i] <- the_value
}
my_results

## EXERCISE: Write a for loop to go through values 4, 16, 25, 36, 49, 64 
## and store their square root values in a vector my_results2


## EXAMPLE 2: The following for loop runs the kmeans() function for 
## each k value between 1 and 10, calculates the average
## within cluster sum of squares and
## saves the result into a vector named avgdis.
avgdis <- c()
my_values <- 1:10
set.seed(0)
for(i in 1:length(my_values)) {
  k_value <- my_values[i]
  km<- kmeans(util_data_std, centers = k_value)
  avgdis[i] <- mean(km$withinss)
}
avgdis

## We can plot these values to see which k is the best value.
## This graph is called an elbow chart.
plot(my_values, avgdis,
     xlab="Number of clusters, k",
     ylab="Average within-clusters sum of squares")
