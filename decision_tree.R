# Load libraries
library(caret)
library(dplyr)
library(ggplot2)
library(pROC)
library(rpart.plot)

# Load the dataset
df <- read.csv("bankmarketing/bank.csv", sep = ';')
str(df)

# Data cleaning and standardization -----------------------------------------------------------

# Change the variable pdays into a binary variable (1 if contacted before, 0 if not contacted)
df <- df %>%
  mutate(
    contacted_before = ifelse(pdays > -1, 1, 0)
  ) %>%
  select(-pdays)

# factorize categorical variables
df$job <- as.factor(df$job)
df$marital <- as.factor(df$marital)
df$education <- as.factor(df$education)
df$default <- as.factor(df$default)
df$housing <- as.factor(df$housing)
df$loan <- as.factor(df$loan)
df$contact <- as.factor(df$contact)
df$month <- as.factor(df$month)
df$poutcome <- as.factor(df$poutcome)
df$y <- as.factor(df$y)

# NB: scaling not necessary for decision trees

# Split data into training and test sets
set.seed(42)
trainIndex <- createDataPartition(df$y, p = 0.8, list = FALSE)
train <- df[trainIndex, ]
test <- df[-trainIndex, ]


# Create the model --------------------------------------------------------
grid <- expand.grid(cp = seq(0.001, 0.1, by = 0.01))

tree <- train(y ~ ., data = train, method = "rpart",
              trControl = trainControl(method = "cv", number = 10, sampling = "up"), # sampling up adjusts class y imbalances 
              tuneGrid = grid)

tree

# Visualization of the tree
rpart.plot(tree$finalModel, type = 3, extra = 104, box.palette = "RdBu", fallen.leaves = TRUE)

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

# Model evaluation --------------------------------------------------------
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
    title = "Confusion Matrix",
    x = "Actual Class",
    y = "Predicted Class"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# ROC - AUC curve
prob <- predict(tree, newdata = test, type = "prob")
roc_curve <- roc(test$y, prob[, 2])
auc_value <-auc(roc_curve)
plot(roc_curve, col = "blue", main = paste("ROC - AUC curve:", round(auc_value, 2)))
