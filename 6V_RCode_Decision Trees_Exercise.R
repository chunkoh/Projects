##################################################################
#  6V: Decision Trees
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
#### EXERCISE: BUILDING A CLASSIFICATION TREE
##================================================================
## We will use the ebayAuctions.csv. 
## Dataset is on Canvas, do not forget to set your directory from 
## Session/Set Working Directory/Choose Directory
ebay_data <- read.csv(file = 'ebayAuctions.csv', header = T, as.is = T)

## Target variable: Competitive

#### Q1: Please read the documentation to learn about the dataset.
#### Which variable(s) cannot be used as a predictor for the 
#### target variable Competitive? (Hint: Which variable(s) are 
#### unknown until the end of the auction?)
#Close Price and End Day

#### Q2: Partition your dataset into training and validation sets
#### as 80% training and 20% validation. Use argument 500 for 
#### set.seed
ebay_size <- 0.8 * nrow(ebay_data)
set.seed(500)
ebay_index <- sample(x = 1:nrow(ebay_data), size = ebay_size, replace = F)
train_set <- ebay_data[ebay_index, ]
valid_set <- ebay_data[-ebay_index, ]

#### Q3: Fit a classification tree using the training
#### dataset and all predictors. Do not forget to exclude
#### predictors that cannot be used as predictors from
#### your training set. Call your fitted tree model cl_tree1.
#### Generate the tree diagram.
train_set <- train_set[,-6]
install.packages('rpart')
library(rpart)
cl_tree_small <- rpart(formula = Competitive ~ . , 
                       data = train_set, method = "class")
install.packages('rpart.plot')
library(rpart.plot)
prp(cl_tree_small, type = 1, extra = 1)


#### Q4: Consider an auction with following category and settings:
#### Category: Music/Movie/Game, Currency: US, SellerRating: 3249,
#### Duration: 5, EndDay: Mon, OpenPrice: 0.01.
#### If we use a cutoff of 0.5, what your tree would return as its class?
#### What if the cutoff is 0.8?
460/(460+119)

#### Q5: Calculate the probability of being competitive for the auctions
#### in the validation set. 
## tree.
pred_loan_tree_prob <- predict(object = cl_tree_small, newdata = valid_set, 
                               type = "prob")


#### Q6: Classify the observations in the validation set using
#### your model and cutoff of 0.5.
cutoff <- 0.5
pred_loan_tree <- ifelse(pred_loan_tree_prob[ ,2]>cutoff, yes = 1, no = 0)

#### Q7: Generate the confusion matrix for the validation set
#### assuming that both classes are equally important (HINT:
#### This means that we will look at the accuracy, so the
#### the value you give to positive does not matter.)
#### What is the accuracy of your tree model?
library(caret)
actual_valid <- valid_set$Competitive
confusionMatrix(as.factor(pred_loan_tree), 
                as.factor(actual_valid), positive = "1")

