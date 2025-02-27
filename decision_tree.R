# Load libraries
library(caret)
library(dplyr)
library(ggplot2)
library(pROC)

# Load the dataset
df <- read.csv("bankmarketing/bank.csv", sep = ';')
str(df)

# Data cleaning and standardization -----------------------------------------------------------

# factorize cataegorical variables
df$job <- as.factor(df$job)
df$marital <- as.factor(df$marital)
df$education <- as.factor(df$education)
df$default <- as.factor(df$default)
df$housing <- as.factor(df$housing)
df$loan <- as.factor(df$loan)
df$contact <- as.factor(df$contact)
df$month <- as.factor(df$month)
df$poutcome <- as.factor(df$poutcome)
df$y <- as.factor(df$y)

# NB: scaling not necessary for decision trees

# Think about Class imbalance, if it's possible to implement it

# Split data into training and test sets
set.seed(42)
trainIndex <- createDataPartition(df$y, p = 0.8, list = FALSE)
train <- df[trainIndex, ]
test <- df[-trainIndex, ]


# Create the model --------------------------------------------------------
tree <- train(y ~ ., data = train, method = "rpart", 
                  trControl = trainControl(method = "cv", number = 10))  # 10-fold cross-validation

# View the results
tree


