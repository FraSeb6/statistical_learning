library(caret)
library(dplyr)
library(ggplot2)
library(pROC)

# Dataset
df <- read.csv("bankmarketing/bank.csv", sep = ';')
View(df)

# Data Cleaning and standardization -----------------------------------------------------------

## Convert yes/no in 1s and 0s
df <- df %>%
  mutate(
    default = ifelse(default == "yes", 1, 0),
    housing = ifelse(housing == "yes", 1, 0),
    loan = ifelse(loan == "yes", 1, 0),
    y = ifelse(y == "yes", 1, 0),
    contacted_before = ifelse(pdays > -1, 1, 0)
  ) %>%
  select(-pdays)

# One-hot encoding for categorical vairables
dummies <- dummyVars("~ .", data = df, fullRank = TRUE)
df <- predict(dummies, df) %>% as.data.frame()

# Normalization of numerical variables
numeric_cols <- c("age", "balance", "duration", "campaign", "previous")
df[numeric_cols] <- scale(df[numeric_cols])

# division of data in training and test
set.seed(42)
trainIndex <- createDataPartition(df$y, p = 0.8, list = FALSE)
train <- df[trainIndex, ]
test <- df[-trainIndex, ]


# Logistic model---------------------------------------------------------

logistic_model <- glm(y ~ ., data = train, family = binomial())
summary(logistic_model)


# Evaluation of the model -------------------------------------------------

# Test set prediction
test$predicted_prob <- predict(logistic_model, newdata = test, type = "response")
test$predicted_class <- ifelse(test$predicted_prob > 0.5, 1, 0)

# Confusion matrix
conf_matrix <- table(Predicted = test$predicted_class, Actual = test$y)
print(conf_matrix)

# Metrics calculation
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[, 1])
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

cat("Sensitivity:", sensitivity, "\n")
cat("Specifisity:", specificity, "\n")
cat("Accuravy:", accuracy, "\n")

# ROC and AUC curves
roc_curve <- roc(test$y, test$predicted_prob)
auc_value <- auc(roc_curve)
plot(roc_curve, col = "blue", main = paste("ROC - AUC curve:", round(auc_value, 2)))

# Graph of the distribution of y with respect to the last contact duration
ggplot(test, aes(x = duration, fill = factor(y))) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(title = "Distribution of y with respect to the last contact duration",
       x = "Last contact duration", y = "Frequency") +
  scale_fill_manual(values = c("red", "green"), name = "y") +
  theme_minimal()