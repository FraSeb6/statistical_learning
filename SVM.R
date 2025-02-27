# Load required libraries
library(gbm)
library(caret)
library(dplyr)
library(ggplot2)
library(reshape2)
library(pROC)
library(e1071)

# Load dataset
data <- read.csv("bankmarketing/bank.csv", sep=";", header=TRUE)

# Convert target variable 'y' to binary (yes = 1, no = 0)
data$y <- ifelse(data$y == "yes", 1, 0)

# Convert categorical variables to factors
categorical_vars <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome")
data[categorical_vars] <- lapply(data[categorical_vars], as.factor)

# Convert 'pdays' to binary variable (contacted before: 1, not contacted: 0)
data$pdays_binary <- ifelse(data$pdays != -1, 1, 0)

# Remove original 'pdays' column
data <- data %>% select(-pdays)

# Move 'y' to the last column
data <- data %>% select(-y, everything(), y)

# Split data into training (80%) and testing (20%) sets
set.seed(42)
trainIndex <- createDataPartition(data$y, p=0.8, list=FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train Support Vector Machine (SVM) Model
svm_model <- svm(y ~ ., data = trainData, kernel = "radial", cost = 1, gamma = 0.1, probability = TRUE)

# Make predictions
svm_pred_probs <- predict(svm_model, testData, probability = TRUE)
svm_predictions <- as.numeric(svm_pred_probs)

# Model Evaluation
conf_matrix_svm <- table(Predicted = svm_predictions, Actual = testData$y)
accuracy_svm <- sum(diag(conf_matrix_svm)) / sum(conf_matrix_svm)

# Visualize Confusion Matrix for SVM
conf_matrix_svm_melted <- melt(conf_matrix_svm)
ggplot(conf_matrix_svm_melted, aes(x = Actual, y = Predicted, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "white", size = 6) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "SVM Confusion Matrix", x = "Actual", y = "Predicted") +
  theme_minimal()

# ROC Curve for SVM
svm_roc_curve <- roc(testData$y, as.numeric(attr(svm_pred_probs, "probabilities")[,2]))
ggplot() +
  geom_line(aes(x = 1 - svm_roc_curve$specificities, y = svm_roc_curve$sensitivities), color = "blue") +
  geom_abline(linetype = "dashed") +
  labs(title = "SVM ROC Curve", x = "1 - Specificity", y = "Sensitivity") +
  theme_minimal()

# Print results
print(conf_matrix_svm)
print(paste("SVM Accuracy:", round(accuracy_svm * 100, 2), "%"))
print(paste("SVM AUC:", round(auc(svm_roc_curve), 4)))

