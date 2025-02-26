# Load libraries
library(caret)
library(dplyr)
library(ggplot2)
library(pROC)

# Load the dataset
df <- read.csv("bankmarketing/bank.csv", sep = ';')

# Data cleaning and standardization -----------------------------------------------------------

# Convert yes/no to valid factor levels
df <- df %>%
  mutate(
    default = ifelse(default == "yes", 1, 0),
    housing = ifelse(housing == "yes", 1, 0),
    loan = ifelse(loan == "yes", 1, 0),
    y = ifelse(y == "yes", "Subscribed", "Not_subscribed"),  # Use valid names for y
    contacted_before = ifelse(pdays > -1, 1, 0)
  ) %>%
  select(-pdays)

# One-hot encoding for categorical variables (excluding y)
dummies <- dummyVars("~ . - y", data = df, fullRank = TRUE)
df_encoded <- predict(dummies, df) %>% as.data.frame()

# Add y back as a factor with valid names
df_encoded$y <- factor(df$y, levels = c("Not_subscribed", "Subscribed"))

# Normalize numerical variables
numeric_cols <- c("age", "balance", "duration", "campaign", "previous")
df_encoded[numeric_cols] <- scale(df_encoded[numeric_cols])

# Split data into training and test sets
set.seed(42)
trainIndex <- createDataPartition(df_encoded$y, p = 0.8, list = FALSE)
train <- df_encoded[trainIndex, ]
test <- df_encoded[-trainIndex, ]

# Logistic regression model -----------------------------------------------------

# Set up 10-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

# Train logistic regression model
logistic_model <- train(y ~ ., data = train, method = "glm", family = binomial(), trControl = ctrl)

# Model summary
summary(logistic_model)

# Model evaluation -----------------------------------------------------------------------

#Predictions on the test set
test$predicted_prob <- predict(logistic_model, newdata = test, type = "prob")[, "Subscribed"]
test$predicted_class <- ifelse(test$predicted_prob > 0.5, "Subscribed", "Not_subscribed")

# Use caret's confusionMatrix function
conf_matrix <- confusionMatrix(factor(test$predicted_class, levels = c("Not_subscribed", "Subscribed")), test$y)
print(conf_matrix)

# Extract and print key metrics
cat("Sensitivity:", conf_matrix$byClass["Sensitivity"], "\n")
cat("Specificity:", conf_matrix$byClass["Specificity"], "\n")
cat("Accuracy:", conf_matrix$overall["Accuracy"], "\n")

# Calculate and print Matthews Correlation Coefficient (MCC)
TP <- as.numeric(conf_matrix$table[2, 2])
TN <- as.numeric(conf_matrix$table[1, 1])
FP <- as.numeric(conf_matrix$table[2, 1])
FN <- as.numeric(conf_matrix$table[1, 2])

MCC <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
cat("MCC:", MCC, "\n")

# ROC curve and AUC
roc_curve <- roc(as.numeric(test$y) - 1, test$predicted_prob)
auc_value <- auc(roc_curve)
plot(roc_curve, col = "blue", main = paste("ROC - AUC curve:", round(auc_value, 2)))
