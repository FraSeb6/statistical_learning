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


# random forest regression model -----------------------------------------------------

# Set up cross-validation with SMOTE
ctrl <- trainControl(
  method = "cv", 
  number = 5, 
  sampling = "smote",
  classProbs = TRUE  # Required for ROC calculation
)

# Define a tuning grid for hyperparameter optimization
tune_grid <- expand.grid(mtry = seq(2, 10, by = 2))  # Try values 2, 4, 6, 8, 10

# Train the Random Forest model with hyperparameter tuning
set.seed(42)
rf_model <- train(
  y ~ ., 
  data = train, 
  method = "rf",
  trControl = ctrl,
  metric = "Accuracy",  # Optimize for accuracy
  tuneGrid = tune_grid  # Use custom hyperparameter tuning
)

# Print training results
print(rf_model)

# Best hyperparameter (mtry)
best_mtry <- rf_model$bestTune$mtry
cat("\nBest mtry:", best_mtry, "\n")

# Calculate and print the average accuracy from cross-validation
avg_accuracy <- mean(rf_model$resample$Accuracy)
cat("\nAverage Accuracy:", round(avg_accuracy, 4), "\n")

# Predict on the test set
rf_pred <- predict(rf_model, test)

# Confusion Matrix
conf_matrix <- confusionMatrix(rf_pred, test$y)
print(conf_matrix)
conf_matrix_table <- as.data.frame(conf_matrix$table)
colnames(conf_matrix_table) <- c("Predicted", "Actual", "Frequency")

TP <- as.numeric(conf_matrix$table[2, 2])
TN <- as.numeric(conf_matrix$table[1, 1])
FP <- as.numeric(conf_matrix$table[2, 1])
FN <- as.numeric(conf_matrix$table[1, 2])
MCC <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
cat("MCC:", MCC, "\n")

# Predict probabilities for ROC curve
rf_prob <- predict(rf_model, test, type = "prob")

# Compute ROC curve
roc_curve <- roc(response = test$y, predictor = rf_prob[,2], levels = rev(levels(test$y)))

# Print AUC value
auc_value <- auc(roc_curve)
cat("\nROC AUC:", round(auc_value, 4), "\n")

# Plot ROC Curve
plot(roc_curve, col = "blue", main = "ROC Curve - Random Forest")

