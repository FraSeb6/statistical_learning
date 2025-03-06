# Load necessary libraries
library(dplyr)
library(caret)
library(ggplot2)
library(car)
library(mltools)
library(pROC)
library(randomForest)

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


# Model Estimation --------------------------------------------------------
# Let's try to find the best model based on mtry

oob.err = double(7)  # Out-of-Bag error
test.err = double(7) # Test error

for(mtry in 1:7) {
  # Fit the Random Forest model with the current mtry value
  fit = randomForest(Survived ~ ., data = train, mtry = mtry, ntree = 1000)
  # Store the final Out-of-Bag error (classification case)
  oob.err[mtry] = fit$err.rate[1000, 1]
  # Make predictions on the test dataset
  pred = predict(fit, newdata = test)
  # Calculate the misclassification rate on the test set
  test.err[mtry] = mean(pred != test$Survived)
  # Print progress and error rates
  cat("mtry =", mtry, "| OOB Error:", oob.err[mtry], "| Test Error:", test.err[mtry], "\n")
}

# Visualize the test and OOB error
matplot(1:7, cbind(test.err, oob.err), pch = 19, type = "b",
        lty = c(1, 2), # Different line types (solid for test, dashed for OOB)
        col = c("red", "blue"), # Red for test error, blue for OOB error
        ylab = "Error Rate", xlab = "mtry",
        main = "OOB vs Test Error (Random Forest)")
legend("bottomright",
       legend = c("Test Error (red - solid)", "OOB Error (blue - dashed)"),
       col = c("red", "blue"), pch = 19, lty = c(1, 2),
       cex = 0.7)

# best model based on the info
rf <- randomForest(Survived ~ ., data = train, mtry=2)

# Get the variable importance from the Random Forest model
var_importance_rf <- sort(rf$importance[, 1], decreasing = TRUE)

barplot(
  var_importance_rf,
  main = "Variable Importance (Random Forest)",
  ylab = "Importance Score",
  col = "lightblue",
  las = 1
)

# Model Evaluation --------------------------------------------------------
test$predictions <- predict(rf, test)

test$predictions <- factor(test$predictions, levels = c("0", "1"), labels = c("Not Survived", "Survived"))
test$Survived <- factor(test$Survived, levels = c("0", "1"), labels = c("Not Survived", "Survived"))

#Confusion Matrix
conf_matrix <- confusionMatrix(test$predictions, test$Survived)

print(conf_matrix)

conf_matrix_table <- as.data.frame(conf_matrix$table)
colnames(conf_matrix_table) <- c("Predicted", "Actual", "Frequency")

ggplot(conf_matrix_table, aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = Frequency), color = "black", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix (Decision Tree)",
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
Survival_Probability <- predict(rf, newdata = test, type = "prob")[,2]
roc_curve <- roc(test$Survived, Survival_Probability)
auc_value <- auc(roc_curve)
plot(roc_curve, col = "blue", main = paste("ROC - AUC Curve (Decision Tree):", round(auc_value, 2)))
