##################################################################
# 2V: Data Exploration
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
#### SOME RSTUDIO KNOWLEDGE      
##
## 1) You can break a line of code and continue on 
## on the next line(s) if it is clear that      
## the expression is not finished yet.  
##
## Example:                                     
1.2 + 2.3 + 3.4 + 4.5 + 5.6 + 
  6.7 + 7.8 + 8.9

## Another example:
c(111111111111111, 2222222222222222222222222222, 
  333333333333333333333333, 444444444444444)

## 2) When you run the code on the first lines only, R is waiting
## for more code. 
## Remember that the symbol > on Console means that R is ready
## for more input. But it is now a plus sign!
## If the symbol on console has changed from '>' to '+',
## you are stuck in continuation mode.
## One way to get out is by clicking on Console and 
## pressing the 'ESC' key.
##
## NOTE: Do NOT break R-variable names or numbers!

## 3) To remove a R-variable you created from R environment,
## use rm(variable_name)
to_be_removed <- 100
rm(to_be_removed)
## To remove ALL R-variables in the R environment, you can
## use the broom in the Environment window

## 4) On the top of your Source window, next to the Run button
## there is another button that says 'Source'. This button runs
## EVERYTHING in your script. It is super useful. For instance, 
## you can click the broom in Environment to delete every R-variable
## then use Source button to check whether your script for a class 
## assignment returns what you want it to return.

##================================================================
#### LOAD THE WEST_ROXBURY DATASET
## Click: Session/Set Working Directory/Choose Directory and 
## choosing the folder directory that .csv file is in.
home_data <- read.csv(file = "K353_West_Roxbury.csv", 
                      header = T, as.is = T)

##================================================================
#### BASIC INFORMATION ABOUT A DATASET
# A quick glance to each variable in the dataset
head(home_data)
str(home_data)
# Report the number of rows and number of columns
dim(home_data) 
nrow(home_data) 
ncol(home_data) 

##================================================================
#### CHECKING THE TYPE OF EACH VARIABLE (COLUMN)
## Quantitative variables must be numerical or integer
## Categorical variables can be text (character), numerical 
## or logical
##
## You can use a str() to obtain the list of variables
## in your dataset and their types
str(home_data)

##================================================================
#### DESCRIBING A NUMERICAL VARIABLE
## Recap- accessing a column in a dataframe
home_data$TOTAL.VALUE
home_data[,"TOTAL.VALUE"] 
home_data[,1] 

## Remember - You can use function colnames() to check
## the name of variables in a dataset
colnames(home_data)

#### SUMMARY STATISTICS
mean(home_data$TOTAL.VALUE) # Mean
median(home_data$TOTAL.VALUE) # Median
max(home_data$TOTAL.VALUE)
min(home_data$TOTAL.VALUE)
max(home_data$TOTAL.VALUE) - min(home_data$TOTAL.VALUE) # Range
sd(home_data$TOTAL.VALUE) # Standard deviation
var(home_data$TOTAL.VALUE) # Variance

#### Exercises ####
# - Can you find the mean and standard deviation values 
#   for the variable "ROOMS"?  
mean(home_data$ROOMS)
sd(home_data$ROOMS)
# - What about mean of YR.BUILT column? Did you get 
#   something different?
mean(home_data$YR.BUILT)

## NOTE: Use the option na.rm to disregard observations 
## with missing values, works with other functions as well
mean(home_data$YR.BUILT, na.rm = TRUE)
var(home_data$YR.BUILT, na.rm = TRUE)

#### HISTOGRAM
## Below code returns the histogram of the total values column
## of our dataset
hist(home_data$TOTAL.VALUE)

## Optional arguments to make your histogram clearer
## main: Title of the histogram
## xlab: x-axis label
## ylab: y-axis label
hist(home_data$TOTAL.VALUE, 
     main = "Histogram of Total Values of Homes",
     xlab = "Total Value",
     ylab = "Count")

#### BOXPLOT
## Below code returns the box plot of the total values column
## of our dataset
boxplot(home_data$TOTAL.VALUE)

## Optional arguments to make your boxplot clearer
## main: Title of the histogram
## xlab: x-axis label
## ylab: y-axis label
boxplot(home_data$TOTAL.VALUE, 
     main = "Box Plot of Total Values of Homes",
     ylab = "Total Value")

##================================================================
#### DESCRIBING A CATEGORICAL VARIABLE
## table() function returns a table that summarizes the count of
## observations for each category
table(home_data$REMODEL)

## Below code returns the bar chart of the remodel column
## of our dataset
barplot(table(home_data$REMODEL))

## Optional arguments to make your bar chart nicer
## main: Title of the chart
## xlab: x-axis label
## ylab: y-axis label
barplot(table(home_data$REMODEL), 
     main = "Bar Chart of Remodeling Information",
     xlab = "Remodeled When",
     ylab = "Count")

##================================================================
#### DESCRIBING RELATIONSHIP BETWEEN TWO NUMERICAL VARIABLES
##
#### SCATTER PLOTS
## plot() function takes two quantitative variables you want to 
## plot against as input and creates a scatter plot
## Variable you want on the y-axis, Variable you want on the 
## x-axis
plot(home_data$LOT.SQFT, home_data$TOTAL.VALUE)

## Optional arguments to make your scatter plot nicer
## main: Title of the plot
## xlab: x-axis label
## ylab: y-axis label
plot(home_data$LOT.SQFT, home_data$TOTAL.VALUE, 
     main = "Scatter Plot between Values and Area",
     xlab = "Lot Size",
     ylab = "Total Value")

#### CORRELATION COEFFICIENTS
## cor() function takes the variables you want to compare as input
## and returns the correlation coefficient 
cor(home_data$TOTAL.VALUE, home_data$GROSS.AREA)

## You can also input multiple variables to get a matrix of 
## correlation coefficients
cor(home_data[ , 1:13], use="complete.obs")

## NOTE: The option use="complete.obs" disregards observations 
##      with missing values, works with cov() function
cor(home_data[ , 1:13]) # returns NAs without use="complete.obs"

## NOTE 2: Rounding up the numbers
round(cor(home_data[,1:13], use="complete.obs"),2)

##================================================================
#### DESCRIBING RELATIONSHIP BETWEEN NUMERICAL VS CATEGORICAL VARIABLES
## boxplot() function can be used to create side by side bar plots
## First argument is numerical and second one is categorical
boxplot(home_data$TOTAL.VALUE ~ home_data$REMODEL)

## Optional arguments to make your boxplot nicer
## main: Title of the plot
## xlab: x-axis label
## ylab: y-axis label
boxplot(home_data$TOTAL.VALUE ~ home_data$REMODEL, 
        main = "Box Plot of Total Values of Homes for Different Remodeling Information",
        xlab = "Remodeling Information",
        ylab = "Total Value")