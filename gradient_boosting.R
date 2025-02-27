# Load required libraries
library(gbm)
library(caret)
library(dplyr)
library(ggplot2)
library(reshape2)
library(pROC)

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

# Train Gradient Boosting Model
gbm_model <- gbm(y ~ ., data = trainData, distribution = "bernoulli", n.trees = 100,
                 interaction.depth = 3, shrinkage = 0.1, cv.folds = 5, n.minobsinnode = 10)

# Make predictions
pred_probs <- predict(gbm_model, testData, n.trees = 100, type = "response")
predictions <- ifelse(pred_probs > 0.5, 1, 0)

# Model Evaluation
conf_matrix <- table(Predicted = predictions, Actual = testData$y)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Visualize Confusion Matrix
conf_matrix_melted <- melt(conf_matrix)
ggplot(conf_matrix_melted, aes(x = Actual, y = Predicted, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black", size = 6) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
  theme_minimal()

# Additional Visualizations
# Distribution of target variable
ggplot(data, aes(x = as.factor(y))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Target Variable", x = "Subscription (y)", y = "Count") +
  theme_minimal()

# Age distribution
ggplot(data, aes(x = age)) +
  geom_histogram(fill = "green", bins = 30, alpha = 0.7) +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal()

# Balance vs. Target variable
ggplot(data, aes(x = as.factor(y), y = balance, fill = as.factor(y))) +
  geom_boxplot() +
  labs(title = "Balance Distribution by Target Variable", x = "Subscription (y)", y = "Balance") +
  theme_minimal()

# ROC Curve
roc_curve <- roc(testData$y, pred_probs)
ggplot() +
  geom_line(aes(x = 1 - roc_curve$specificities, y = roc_curve$sensitivities), color = "blue") +
  geom_abline(linetype = "dashed") +
  labs(title = "ROC Curve", x = "1 - Specificity", y = "Sensitivity") +
  theme_minimal()

# Print results
print(conf_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))
print(paste("AUC:", round(auc(roc_curve), 4)))
