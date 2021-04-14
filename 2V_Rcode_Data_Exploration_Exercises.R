##################################################################
# 2V: Data Exploration
##################################################################

##================================================================
#### DATA EXPLORATION EXERCISES
## Please load the donations.csv dataset (available on Canvas). 
new_data <- read.csv(file= "donations.csv", header = T, as.is = T)
## 1. How many rows and columns of data does it contain?
ncol(new_data)
nrow(new_data)
dim(new_data)
## 2. Take a quick glance to each variable in the dataset (for 
#  instance, get R to display the first 6 rows of data), also take
#  a look at the documentation about the dataset. Do you agree
#  with the data types specified? 
str(new_data[1:6,])

## 3. Please report the mean, max, min and standard deviation for the 
#  following variables: GiftCntAll GiftAvgAll DemAge
mean(new_data[,"GiftCntAll"])
mean(new_data[,"GiftAvgAll"])
mean(new_data[,"DemAge"])
max(new_data[,"GiftCntAll"])
max(new_data[,"GiftAvgAll"])
max(new_data[,"DemAge"])
min(new_data[,"GiftCntAll"])
min(new_data[,"GiftAvgAll"])
min(new_data[,"DemAge"])
sd(new_data[,"GiftCntAll"])
sd(new_data[,"GiftAvgAll"])
sd(new_data[,"DemAge"])


## 4. What is the correlation coefficient between GiftAvgAll 
#  and GiftCntAll? What does it imply? 
cor(new_data$GiftAvgAll,new_data$GiftCntAll)

## 5. Create a histogram and a boxplot for GiftCntAll.
hist(new_data$GiftCntAll)
boxplot(new_data$GiftCntAll)

## 6. What do you observe from these plots?
## Right Skewed

## 7. Create a histogram and a boxplot for DemMedIncome.
#  Is there anything that caught your attention? 
hist(new_data$DemMedIncome)
boxplot(new_data$DemMedIncome)
