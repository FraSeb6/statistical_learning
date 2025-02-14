##1- Import and libraries

library(tidyverse)  # For data manipulation
library(caret)      # For machine learning functions
library(DataExplorer)  # For quick data visualization
library(nnet)
library(rpart)
library(rpart.plot)
library(randomForest)

# Load the dataset (ensure the file is in your working directory)
df <- read.csv("bankmarketing/bank.csv", sep = ";")  # Adjust separator if needed

# Quick overview of the dataset
str(df)
summary(df)

# Check for missing values
plot_missing(df)

df$y <- as.factor(df$y)  # Convert target variable to a factor



##2- Data Pre-processing

# Convert categorical variables to factors
df <- df %>%
  mutate_if(is.character, as.factor)

# Split into training and test sets (80% train, 20% test)
set.seed(123)
trainIndex <- createDataPartition(df$y, p = 0.8, list = FALSE)
train <- df[trainIndex, ]
test <- df[-trainIndex, ]



##3- Training a Logistic Regression Model

log_model <- glm(y ~ ., data = train, family = "binomial")
summary(log_model)

# Make predictions on the test set
pred_log <- predict(log_model, test, type = "response")
pred_class <- ifelse(pred_log > 0.5, "yes", "no")

# Convert predictions to factor
pred_class <- factor(pred_class, levels = levels(test$y))

# Evaluate performance
confusionMatrix(pred_class, test$y)



##4- Decision Tree Model

# Train decision tree
tree_model <- rpart(y ~ ., data = train, method = "class")
rpart.plot(tree_model)

# Predict on test data
pred_tree <- predict(tree_model, test, type = "class")

# Evaluate
confusionMatrix(pred_tree, test$y)



##5- Random Forest Classifier

# Train Random Forest model
set.seed(123)
rf_model <- randomForest(y ~ ., data = train, ntree = 100)

# Feature importance plot
varImpPlot(rf_model)

# Predict on test data
pred_rf <- predict(rf_model, test)

# Evaluate
confusionMatrix(pred_rf, test$y)

##6- Artificial Neural Networks (ANN)

# Train a neural network with 5 hidden units
ann_model <- nnet(y ~ ., data = train, size = 5, decay = 0.1, maxit = 500)

# Predict on test data
pred_ann <- predict(ann_model, test, type = "class")

# Evaluate
confusionMatrix(factor(pred_ann, levels = levels(test$y)), test$y)



##7- Compare Model Performance
# Function to calculate Accuracy
accuracy <- function(actual, predicted) {
  mean(actual == predicted)
}

# Print accuracy for each model
cat("Logistic Regression Accuracy:", accuracy(test$y, pred_class), "\n")
cat("Decision Tree Accuracy:", accuracy(test$y, pred_tree), "\n")
cat("Random Forest Accuracy:", accuracy(test$y, pred_rf), "\n")
cat("ANN Accuracy:", accuracy(test$y, factor(pred_ann, levels = levels(test$y))), "\n")



