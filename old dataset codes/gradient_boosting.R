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

# gradient boosting regression model -----------------------------------------------------

# Set up cross-validation with SMOTE
ctrl <- trainControl(
  method = "cv", 
  number = 10, 
  sampling = "smote",
  classProbs = TRUE  # Required for ROC calculation
)

# Define a tuning grid for GBM
tune_grid <- expand.grid(
  n.trees = seq(50, 250, by = 50),  # Number of boosting iterations
  interaction.depth = c(1, 3, 5),   # Depth of trees
  shrinkage = c(0.01, 0.1),         # Learning rate
  n.minobsinnode = 10               # Minimum number of observations per leaf
)

# Train the GBM model with hyperparameter tuning
set.seed(42)
gbm_model <- train(
  y ~ ., 
  data = train, 
  method = "gbm",
  trControl = ctrl,
  metric = "Accuracy",  # Optimize for accuracy
  tuneGrid = tune_grid,
  verbose = FALSE
)

# Print training results
print(gbm_model)

# Best hyperparameters
best_params <- gbm_model$bestTune
print(best_params)

# Calculate and print the average accuracy from cross-validation
avg_accuracy <- mean(gbm_model$resample$Accuracy)
cat("\nAverage Accuracy:", round(avg_accuracy, 4), "\n")

# Predict on the test set
gbm_pred <- predict(gbm_model, test)

# Confusion Matrix
conf_matrix <- confusionMatrix(gbm_pred, test$y)
print(conf_matrix)

# Predict probabilities for ROC curve
gbm_prob <- predict(gbm_model, test, type = "prob")

# Compute ROC curve
roc_curve <- roc(response = test$y, predictor = gbm_prob[,2], levels = rev(levels(test$y)))

# Print AUC value
auc_value <- auc(roc_curve)
cat("\nROC AUC:", round(auc_value, 4), "\n")

# Plot ROC Curve
plot(roc_curve, col = "blue", main = paste("ROC Curve - AUC (Gradient Boosting):", round(auc_value, 2)))

# --- Confusion Matrix Heatmap --
# Convert confusion matrix to a dataframe
cm_data <- as.data.frame(conf_matrix$table)
colnames(cm_data) <- c("Predicted", "Actual", "Count")

# Ensure correct order of Actual and Predicted labels
cm_data$Actual <- factor(cm_data$Actual, levels = rev(levels(test$y)))
cm_data$Predicted <- factor(cm_data$Predicted, levels = rev(levels(test$y)))

# Plot the confusion matrix heatmap
ggplot(cm_data, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "black", size = 6) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Confusion Matrix - Gradient Boosting", x = "Actual", y = "Predicted")
