library(caret)
library(dplyr)
library(ggplot2)
library(pROC)
library(rpart.plot)

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


# Create the model --------------------------------------------------------
grid <- expand.grid(cp = seq(0.001, 0.1, by = 0.01))

tree <- train(y ~ ., data = train, method = "rpart",
              trControl = trainControl(method = "cv", number = 10, sampling = "smote"), 
              tuneGrid = grid)

# Display the complexity parameter table
print(tree$finalModel$cptable)

# Visualization of the tree
rpart.plot(tree$finalModel, type = 3, extra = 104, box.palette = "RdBu", fallen.leaves = TRUE)


# Evaluation --------------------------------------------------------

# Cross-validation performance results from the caret model
cv_results <- tree$results  # This contains accuracy or other metrics for each fold
print(cv_results)

# Plotting the results
ggplot(cv_results, aes(x = cp, y = Accuracy)) +
  geom_line(color = "blue", size = 1) +  # Line for accuracy
  geom_point(color = "red", size = 2) +  # Points for each model
  labs(
    title = "Cross-Validation Results",
    x = "Complexity Parameter (cp)",
    y = "Accuracy"
  ) +
  theme_minimal()

# Show variable importance
var_imp <- varImp(tree)$importance

var_imp$Variable <- rownames(var_imp)

top_vars <- var_imp %>%
  arrange(desc(Overall)) %>%
  head(7)

ggplot(top_vars, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  # Flip axes for better label spacing
  theme_minimal() +
  labs(title = "Most Important Variables", x = "Features", y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5))

# Predictions
predictions <- predict(tree, newdata = test)

# Confusion Matrix 
conf_matrix <- confusionMatrix(predictions, test$y)
print(conf_matrix)
conf_matrix_table <- as.data.frame(conf_matrix$table)
colnames(conf_matrix_table) <- c("Predicted", "Actual", "Frequency")

ggplot(conf_matrix_table, aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = Frequency), color = "black", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix (decision tree)",
    x = "Actual Class",
    y = "Predicted Class"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# ROC - AUC curve
prob <- predict(tree, newdata = test, type = "prob")
roc_curve <- roc(test$y, prob[, 2])
auc_value <-auc(roc_curve)
plot(roc_curve, col = "blue", main = paste("ROC - AUC curve (decision tree):", round(auc_value, 2)))

# pruned tree -------------------------------------------------------------

# Set the tuning grid with your desired cp value
cp_value <- 0.0021
tune_grid <- expand.grid(cp = cp_value)

# Train the decision tree with caret and prune at cp = 0.021
pruned <- train(
  y ~ ., 
  data = train, 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10), # 10-fold cross-validation
  tuneGrid = tune_grid # Use the specific cp value
)

# Visualize the pruned tree
rpart.plot(pruned$finalModel)

# Evaluate the pruned model
cv_results <- pruned$results  # This contains accuracy or other metrics for each fold
print(cv_results)
predictions_pruned <- predict(pruned, test)
conf_matrix_pruned <- confusionMatrix(predictions_pruned, test$y)
print(conf_matrix_pruned)

# Confusion Matrix visualization
conf_matrix_pruned <- confusionMatrix(predictions_pruned, test$y)
print(conf_matrix_pruned)
conf_matrix_table_pruned <- as.data.frame(conf_matrix$table)
colnames(conf_matrix_table_pruned) <- c("Predicted", "Actual", "Frequency")

ggplot(conf_matrix_table_pruned, aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = Frequency), color = "black", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix (pruned tree)",
    x = "Actual Class",
    y = "Predicted Class"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# ROC - AUC curve
prob <- predict(pruned, newdata = test, type = "prob")
roc_curve <- roc(test$y, prob[, 2])
auc_value <-auc(roc_curve)
plot(roc_curve, col = "blue", main = paste("ROC - AUC curve (pruned tree):", round(auc_value, 2)))
