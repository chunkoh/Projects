##################################################################
#  2: Datasets in R
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
#### VECTORS (A WAY TO STORE MORE THAN ONE VALUE IN AN R-VARIABLE)
## What if we want to hold multiple values in one R-variable?
## 
## A vector is a finite sequence of values.
##
## For this, we use the function c(). 
## c stands for concatenate or collect or combine.
c(344.2, 412.6, 330.1, 498.6, 331.5)

## c() created a vector of size 5 with the values we provided.

## Let's assign this vector to an R-variable
new_vector <- c(344.2, 412.6, 330.1, 498.6, 331.5)
new_vector

## Look at Environment panel. We have a new R-variable.

#### Exercises ####
# - Create a numeric vector with name another_vector (name of
#   the R-variable that holds the vector will be another_vector)
#   that holds the following value: 
#   42, 56, 765, 34.2, 1009
another_vector <- c(42,56,765,34.2,1009)

# - Run your R-variable named another_vector in the console
#   to check your work
another_vector
# - Do these three following lines work? What are the outputs?
#   What is your takeaway?
hello_1 <- c(0, c(1,2), c(3,4,5))
hello_2 <- c(0,1,2,3,4,5)
hello_3 <-c(0,c(1,c(2,c(3,c(4,5)))))

hello_1

#### More Exercises ####
# - Here is a vector containing heights of different 
#   students in cms
student_heights <- c(155, 190, 172, 183)
#   We obtained the height data from 3 more students, 
new_student_heights <- c(160, 189, 180)
#   Merge these two vectors into one vector.
c(student_heights ,new_student_heights)

student_heights
# - Create an R-variable named ba_courses that holds the 
#   following text values: 'K327', 'K353', 'G350', 'G492'
ba_courses <- c('K327', 'K353', 'G350', 'G492')
# - What happens when you run the following line of code?
#   What is your takeaway?
c('K353', 10)
# c only contains one type of value, it cannot be mixed of text and number
# - What about the following line of code?
c('K353', 10, NA)

##============================================================
#### A SPECIAL WAY TO CREATE NUMERICAL VECTORS
## The operator ':' creates a sequence of numbers with a
## space of 1 in between.
##
## To create a sequence starting at 1, ending at 100, 
## spaced by 1 as a vector
1:100

## More examples:
10:5
1.4:9

## Show all integers from 901 to 999:
901:999

##================================================================
#### INTRODUCING DATA FRAMES
## Data frames are the most frequently used type of object to store
## data in R. You can think of data frame as a collection of vectors
## side by side, where the columns are vectors of equal length.
## - The rows characterize observations (samples, records).
## - The columns represent variables (fields).
##
## Each variable/column is a vector. Remember that vectors can hold
## only one type of value. Therefore, each 
## column must contain only one basic data type (numeric, 
## logical, text). (Recall that missing value can occur with any
## basic value type, and special numerical values like Inf and NaN
## are considered numeric.)
##
## Rows are different in that they are not 
## vectors because their elements are allowed to be of different 
## types.

##================================================================
#### IMPORTING DATA FRAMES INTO R
##
## Often a dataset originated as an Excel spreadsheet. As an example
## I uploaded a dataset named K353_West_Roxbury.csv
## to Canvas. First, download it to your computer.
## 
## A CSV file means "comma-separated values" and has the extension 
## ".csv". It is human-readable, so you can open it with Notepad 
## or Excel. Make sure to keep saving it as .csv.
##
## To import it in R, do the following:
## 1) Save it to a folder of your choosing. I recommend saving it
## in the same folder with your R file.

## 2) Make sure the file is in R's working directory. You can find
## the working directory by asking R the following:
getwd()
## And you can change the working directory if needed by either
## entering the path you want into the following function
setwd("C:/Users/ozgey/OneDrive - Indiana University/FromBox/Teaching/K353-Spring2021/Materials per topic/1V&2 - Business Understanding, Data Understanding, R Basics and Datasets")
## or going to Session/Set Working Directory/Choose Directory and 
## choosing the folder directory that .csv file is in. 

## 3) Read the CSV file with the function 'read.csv()' as follows:
home_data <- read.csv(file = "K353_West_Roxbury.csv", 
                      header = T, as.is = T)

## Arguments of read.csv function mean:
## file: text, name of the file you want to upload, make sure 
##       that your file is in the source directory
## header: logical, indicates whether the file contains the 
##       names of the variables as its first line
## as.is: logical, default behavior of R is to convert text
##        values to something else. It is better to upload data
##        as it is, and do the conversion yourself if needed. So,
##        I suggest always setting it to TRUE.
##
## NOTE: You can also click File -> Import Datasets to import other
## types of data, e.g. text, excel, sas, stata
##
##================================================================
#### GETTING TO KNOW A DATAFRAME
## Following functions are useful to get same basic information
## about a dataframe
str(home_data) # A quick glance to each variable in the dataset
summary(home_data) #summary statistics of each variable
dim(home_data) # Number of rows and number of columns
nrow(home_data) # Number of rows
ncol(home_data) # Number of columns
colnames(home_data) # Names of columns
head(home_data) # displays the first six rows
View(home_data) # display the data frame in a new tab

##================================================================
#### ACCESSING A VARIABLE (COLUMN) IN A DATA FRAME
## Below, three approaches return the total value column of the 
## data frame home_data
##
#### APPROACH 1: USE BRACKETS WITH INDICES! Remember that data frames  
## have two dimensions. The first element in the brackets is the row 
## index, and the second one is the column index. 
## dataframe[rowIDs, columnIDs]
## Blank rowIDs means all observations are returned.

## Following returns the tax variable because that's the second column
home_data[ , 2] 

#### APPROACH 2: USE BRACKETS WITH COLUMN NAMES!
## Remember colnames()?
colnames(home_data)
## Following also returns the tax column
home_data[,"TAX"] 

#### APPROACH 3: USE SYMBOL $ WITH COLUMN NAMES!
home_data$TOTAL.VALUE

#### Exercises ####
# - Display the values for the "YR.BUILT" variable?
home_data$YR.BUILT
##================================================================
#### ACCESSING AN OBSERVATION (ROW) IN A DATA FRAME
## Again, USE BRACKETS! Remember dataframe[rowIDs, columnIDs] 
## Blank columnIDs means all columns are returned.
home_data[1, ] # first observation
home_data[1:10, ] #first 10 observations

##================================================================
#### ACCESSING A SUBSET OF A DATA FRAME
## 
## EXAMPLE 1:
home_data[1, 1]  #first row of first column
home_data[1:3, 1] #first three rows of first column
home_data[c(5,3), 1] #fifth and third rows of first colum

## EXAMPLE 2:
## Following two return the total value and year built variables of 
## first 10  rows 
home_data[1:10,c("TOTAL.VALUE","YR.BUILT")]
home_data[1:10,c(1,4)]

## EXAMPLE 3:
## Only interested in one variable? Maybe try the following
home_data$TOTAL.VALUE[1:10] #first 10 rows of total value
## home_data$TOTAL.VALUE[1:10] is same as the following two
home_data[1:10,1]
home_data[1:10,"TOTAL.VALUE"]

## EXAMPLE 4: Elements can be deselected by using negative 
## indices
home_data$TOTAL.VALUE[-1] #all except the first
home_data$TOTAL.VALUE[-(1:3)] #all except the first three
home_data[, -14] #all except the 14th column, same as home_data[, 1:13]
home_data[-(1:5),] # all expect the first 5 rows

#### Exercises ####
# - Save the rooms variable value of first 10 observations
#   to a R-variable named room_10
room_10 <- home_data[1:10,"ROOMS"]
room_10 <- home_data[1:10,8]
room_10 <- home_data$ROOMS[1:10]
# - Display the lot square ft value of first observation
home_data[1,"LOT.SQFT"]
# - Display the tax values of third and second
#   observations in that order
home_data[c(3,2),"TAX"]
# - Display all columns of first 10 rows except TOTAL.VALUE 
#   and TAX (1st and 2nd)
home_data[1:10,c(-1,-2)]
