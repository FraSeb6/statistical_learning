# Load necessary libraries
library(dplyr)
library(caret)
library(ggplot2)
library(car)
library(mltools)
library(pROC)

# Load the datasets
titanic <- read.csv("titanic_combined.csv")

# Data cleaning -----------------------------------------------------------
# drop useless columns
titanic <- titanic[, !names(titanic) %in% c("PassengerId", "Name", "Ticket", "Cabin")]

titanic[titanic == ""] <- NA
# Convert categorical variables to factors
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)
titanic$Survived <- as.factor(titanic$Survived)

# Impute missing values ---------------------------------------------
# Function to calculate the mode (most frequent value)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Impute Age with median
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)

# Impute Embarked with mode
titanic$Embarked[is.na(titanic$Embarked)] <- get_mode(titanic$Embarked)

# Impute Fare with median
titanic$Fare[is.na(titanic$Fare)] <- median(titanic$Fare, na.rm = TRUE)

# Standardize Numeric columns
numeric_columns <- titanic[, c("Age", "Fare", "SibSp", "Parch")]
titanic_standardized <- scale(numeric_columns)
titanic_standardized <- as.data.frame(titanic_standardized)
titanic[, c("Age", "Fare", "SibSp", "Parch")] <- titanic_standardized

# Create a partition (80% for training, 20% for testing)
set.seed(123)  # For reproducibility
train_index <- createDataPartition(titanic$Survived, p = 0.8, list = FALSE)

# Split the data into training and testing sets
train <- titanic[train_index, ]
test <- titanic[-train_index, ]

# Model creation ----------------------------------------------------------

# Train logistic regression model using caret for classification
set.seed(123)
logit_model <- train(Survived ~ ., data = train, method = "glm", family = binomial,
                     trControl = trainControl(method = "cv", number = 10), 
                     metric = "Accuracy")

# Summary of the model
summary(logit_model$finalModel)


# Evaluation of the Model -------------------------------------------------
# cross-validation evaluation 
print(logit_model$results$Accuracy)

# Predict on test dataset
test$Survival_Probability <- predict(logit_model, newdata = test, type = "prob")[,2]

# Convert probabilities to binary outcomes (threshold = 0.5)
test$predictions <- ifelse(test$Survival_Probability > 0.5, 1, 0)
test$predictions <- as.factor(test$predictions)

# Evaluate model performance
# Now compute the confusion matrix
conf_matrix <- confusionMatrix(test$predictions, test$Survived)

# Print the confusion matrix
print(conf_matrix)

conf_matrix_table <- as.data.frame(conf_matrix$table)
colnames(conf_matrix_table) <- c("Predicted", "Actual", "Frequency")

ggplot(conf_matrix_table, aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = Frequency), color = "black", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix (logistic)",
    x = "Actual Class",
    y = "Predicted Class"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# Compute Matthews Correlation Coefficient (MCC)
predictions <- as.numeric(predict(logit_model, newdata = test))
actuals <- as.numeric(test$Survived)
MCC <- mcc(preds = predictions, actuals = actuals)
print(paste("Matthews Correlation Coefficient (MCC):", MCC))

# ROC-AUC curve
roc_curve <- roc(test$Survived, test$Survival_Probability)
auc_value <- auc(roc_curve)
plot(roc_curve, col = "blue", main = paste("ROC - AUC curve (logistic):", round(auc_value, 2)))
