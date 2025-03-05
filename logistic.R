<<<<<<< Updated upstream
=======
library(caret)
library(dplyr)
library(ggplot2)
library(pROC)

# Load the dataset
df <- read.csv("bankmarketing/bank.csv", sep = ';')

# Data cleaning and standardization -----------------------------------------------------------

df[df == "unknown"] <- NA

# we are gonna drop the columns with too many unknowns (converted in NA) 
# and the rows having NA values in job and education (the only two having those)

# Convert yes/no to valid factor levels
df <- df %>%
  mutate(
    default = ifelse(default == "yes", 1, 0),
    housing = ifelse(housing == "yes", 1, 0),
    loan = ifelse(loan == "yes", 1, 0),
    y = factor(ifelse(y == "yes", "Subscribed", "Not_subscribed")),
    contacted_before = ifelse(pdays > -1, 1, 0)
  ) %>%
  select(-pdays, -poutcome, -contact, -day)

#eliminate rows with NA values
df_clean <- df[complete.cases(df), ]

# Group job categories (keep as factor)
df_clean <- df_clean %>%
  mutate(
    job_group = factor(case_when(
      job %in% c("blue-collar", "technician", "services", "housemaid") ~ "Manual_Work",
      job %in% c("admin.", "management", "entrepreneur", "self-employed") ~ "White_Collar",
      job %in% c("retired", "student", "unemployed") ~ "Non_Working"
    ))
  ) %>%
  select(-job)

# Group marital status (keep as factor)
df_clean <- df_clean %>%
  mutate(
    marital_group = factor(case_when(
      marital %in% c("married") ~ "Married",
      marital %in% c("single", "divorced") ~ "Single"
    ))
  ) %>%
  select(-marital)

# Group months into quadrimesters (keep as factor)
df_clean <- df_clean %>%
  mutate(
    Q = factor(case_when(
      month %in% c("jan", "feb", "mar", "apr") ~ "1",
      month %in% c("may", "jun", "jul", "aug") ~ "2",
      month %in% c("sep", "oct", "nov", "dec") ~ "3"
    ))
  ) %>%
  select(-month)

# Reordering for clarity
df_clean <- df_clean %>%
  select(-y, everything(), y)

# Normalize numerical variables
numeric_cols <- c("age", "balance", "duration", "campaign", "previous")
df_clean[numeric_cols] <- scale(df_clean[numeric_cols])

# Split data into training and test sets
set.seed(42)
trainIndex <- createDataPartition(df_clean$y, p = 0.8, list = FALSE)
train <- df_clean[trainIndex, ]
test <- df_clean[-trainIndex, ]


# Logistic regression model -----------------------------------------------------

# Set up 10-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10, sampling = "smote")

# Train logistic regression model
logistic_model <- train(y ~ ., data = train, method = "glm", family = binomial(), trControl = ctrl)

# Model summary
summary(logistic_model)

# Model evaluation -----------------------------------------------------------------------

# Cross-validation performance results
print(logistic_model$results$Accuracy)

# Predictions on the test set
test$predicted_prob <- predict(logistic_model, newdata = test, type = "prob")[, "Subscribed"]
test$predicted_class <- ifelse(test$predicted_prob > 0.5, "Subscribed", "Not_subscribed")

# Confusion Matrix
conf_matrix <- confusionMatrix(factor(test$predicted_class, levels = c("Not_subscribed", "Subscribed")), test$y)
print(conf_matrix)

# Create confusion matrix visualization
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

# Calculate and print Matthews Correlation Coefficient (MCC)
TP <- as.numeric(conf_matrix$table[2, 2])
TN <- as.numeric(conf_matrix$table[1, 1])
FP <- as.numeric(conf_matrix$table[2, 1])
FN <- as.numeric(conf_matrix$table[1, 2])
MCC <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
cat("MCC:", MCC, "\n")

# ROC curve and AUC
roc_curve <- roc(test$y, test$predicted_prob)
auc_value <- auc(roc_curve)
plot(roc_curve, col = "blue", main = paste("ROC - AUC curve (logistic):", round(auc_value, 2)))
>>>>>>> Stashed changes
