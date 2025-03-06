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

# In order not to have to standardize we just don't consider the outliers for Fare
max(titanic$Fare, na.rm = TRUE)

titanic <- titanic[!is.na(titanic$Fare) & round(titanic$Fare, 3) != 512.329, ]

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

# Create a partition (80% for training, 20% for testing)
set.seed(123)
train_index <- createDataPartition(titanic$Survived, p = 0.8, list = FALSE)

# Split the data into training and testing sets
train <- titanic[train_index, ]
test <- titanic[-train_index, ]

# Train Random Forest Model -----------------------------------------------
# Create a tuneGrid for Random Forest
n_predictors <- ncol(titanic)-1
mtry_value <- round(sqrt(n_predictors)) 
rf_grid <- expand.grid(mtry = seq(mtry_value - 2, mtry_value + 2))

rf_model <- train(
  Survived ~ ., 
  data = train, 
  method = "rf", 
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = rf_grid,
  metric = "Accuracy"
)

# Print Model Summary
print(rf_model)

# Model Evaluation --------------------------------------------------------
results_df <- as.data.frame(rf_model$results)
ggplot(results_df, aes(x = mtry, y = Accuracy)) +
  geom_point(color = "blue") +
  geom_line(color = "blue") +
  labs(title = "Accuracy vs. mtry", x = "mtry", y = "Accuracy") +
  theme_minimal()

best_accuracy <- rf_model$results[rf_model$bestTune$mtry == rf_model$results$mtry, "Accuracy"]
print(best_accuracy)

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
