# Load necessary libraries
library(dplyr)
library(caret)
library(ggplot2)
library(randomForest)
library(pROC)
library(mltools)

# Load the dataset
titanic <- read.csv("titanic_combined.csv")

# Data Cleaning -----------------------------------------------------------
# Drop unnecessary columns
titanic <- titanic[, !names(titanic) %in% c("PassengerId", "Name", "Ticket", "Cabin")]

titanic[titanic == ""] <- NA

# Convert categorical variables to factors
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)
titanic$Survived <- as.factor(titanic$Survived)

# Impute missing values ---------------------------------------------------
# Function to get mode (most frequent value)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Impute missing values
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)
titanic$Embarked[is.na(titanic$Embarked)] <- get_mode(titanic$Embarked)
titanic$Fare[is.na(titanic$Fare)] <- median(titanic$Fare, na.rm = TRUE)

# Standardize Numeric Columns
numeric_columns <- titanic[, c("Age", "Fare", "SibSp", "Parch")]
titanic_standardized <- scale(numeric_columns)
titanic_standardized <- as.data.frame(titanic_standardized)
titanic[, c("Age", "Fare", "SibSp", "Parch")] <- titanic_standardized

# Split data into training (80%) and testing (20%) ------------------------
set.seed(123)  # For reproducibility
train_index <- createDataPartition(titanic$Survived, p = 0.8, list = FALSE)

train <- titanic[train_index, ]
test <- titanic[-train_index, ]

# Train Random Forest Model -----------------------------------------------
set.seed(123)
rf_model <- train(
  Survived ~ ., 
  data = train, 
  method = "rf", 
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = expand.grid(mtry = sqrt(ncol(train) - 1)),  # Auto-tune 'mtry'
  metric = "Accuracy"
)

# Print Model Summary
print(rf_model)

# Model Evaluation --------------------------------------------------------
# Predictions on test set
test$predictions <- predict(rf_model, newdata = test)

# Confusion Matrix
conf_matrix <- confusionMatrix(test$predictions, test$Survived)
print(conf_matrix)

# Visualizing Confusion Matrix
conf_matrix_table <- as.data.frame(conf_matrix$table)
colnames(conf_matrix_table) <- c("Predicted", "Actual", "Frequency")

ggplot(conf_matrix_table, aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = Frequency), color = "black", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix (Random Forest)",
    x = "Actual Class",
    y = "Predicted Class"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# Compute Matthews Correlation Coefficient (MCC)
predictions <- as.numeric(test$predictions)
actuals <- as.numeric(test$Survived)
MCC <- mcc(preds = predictions, actuals = actuals)
print(paste("Matthews Correlation Coefficient (MCC):", MCC))

# ROC Curve and AUC Score
test$Survival_Probability <- predict(rf_model, newdata = test, type = "prob")[,2]
roc_curve <- roc(test$Survived, test$Survival_Probability)
auc_value <- auc(roc_curve)
plot(roc_curve, col = "blue", main = paste("ROC - AUC Curve (Random Forest):", round(auc_value, 2)))
