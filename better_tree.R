library(dplyr)
library(caret)
library(ggplot2)
library(car)
library(mltools)
library(pROC)
library(rpart)
library(rpart.plot)

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

# Round ages < 1 to 1
titanic$Age[titanic$Age < 1] <- 1
titanic$Age <- round(titanic$Age)

# In order not to have to standardize we just don't consider the outliers for Fare
max(titanic$Fare, na.rm = TRUE)

titanic <- titanic[!is.na(titanic$Fare) & round(titanic$Fare, 3) != 512.329, ]

# Impute missing values
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
set.seed(123)  # For reproducibility
train_index <- createDataPartition(titanic$Survived, p = 0.8, list = FALSE)

# Split the data into training and testing sets
train <- titanic[train_index, ]
test <- titanic[-train_index, ]


# (rpart) Model generation--------------------------------------------------------
train$Survived <- factor(train$Survived, levels = c(0, 1), labels = c("Not Survived", "Survived"))

tree_model<-rpart(Survived ~ ., train)
printcp(tree_model)

plotcp(tree_model)
rpart.plot(tree_model)

# Variance Importance
print(tree_model$variable.importance)
barplot(
  tree_model$variable.importance,
  main = "Variable Importance (Decision Tree)",
  ylab = "Importance Score",
  col = "lightblue",
  las = 1
)

## Model evaluation --------------------------------------------------------
test$Survived <- factor(test$Survived, levels = c(0, 1), labels = c("Not Survived", "Survived"))
test$predictions <- predict(tree_model, newdata = test, type = "class")

#Confusion matrix
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
Survival_Probability <- predict(tree_model, newdata = test, type = "prob")[,2]
roc_curve <- roc(test$Survived, Survival_Probability)
auc_value <- auc(roc_curve)
plot(roc_curve, col = "blue", main = paste("ROC-AUC Curve (Decision Tree - rpart pack):", round(auc_value, 2)))


## COMMENT : implements CART and automatically chooses the best pruning (using cp)
            # accuracy = 0.8769
            # MCC: 0.7362
            # AUC: 0.89

# (tree) model generation --------------------------------------------------------------
library(tree)

tree_model2 <- tree(Survived ~ ., train)

# let's select the best pruning for this tree
set.seed(123)
cv.titanic=cv.tree(tree_model2,FUN=prune.misclass)

# graph of misclass given the size of the tree
plot(cv.titanic)

prune.titanic=prune.misclass(tree_model2,best=4)
plot(prune.titanic);text(prune.titanic,pretty=0)

# Variance Importance
split_data <- tree_model2$frame
split_data <- split_data[split_data$var != "<leaf>", ]

deviance_reduction <- tapply(split_data$dev, split_data$var, sum)
barplot(sort(deviance_reduction, decreasing = TRUE),
        main = "Variable Importance (of the tree model)",
        col = "steelblue",
        las = 2,         # Rotate axis labels
        ylab = "Total Deviance Reduction")

## model evaluation --------------------------------------------------------
test$predictions_2 <- predict(tree_model2, newdata = test, type = "class")

#Confusion matrix
conf_matrix2 <- confusionMatrix(test$predictions_2, test$Survived)
print(conf_matrix2)

conf_matrix_table2 <- as.data.frame(conf_matrix2$table)
colnames(conf_matrix_table2) <- c("Predicted", "Actual", "Frequency")

ggplot(conf_matrix_table2, aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = Frequency), color = "black", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix (Decision Tree using tree pack)",
    x = "Actual Class",
    y = "Predicted Class"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# Compute Matthews Correlation Coefficient (MCC)
predictions_2 <- as.numeric(test$predictions_2)
actuals <- as.numeric(test$Survived)
MCC <- mcc(preds = predictions_2, actuals = actuals)
print(paste("Matthews Correlation Coefficient (MCC):", MCC))

# ROC Curve and AUC Score
Survival_Probability2 <- predict(tree_model2, newdata = test, type = "vector")[,2]
roc_curve2 <- roc(test$Survived, Survival_Probability2)
auc_value2 <- auc(roc_curve)
plot(roc_curve2, col = "blue", main = paste("ROC-AUC Curve (Decision Tree- tree pack):", round(auc_value2, 2)))

#COMMENT: tree package uses a traditional recursive binary partitioning method (similar to CART but simpler).
# using deviation (derived from entropy) as splitting criterion
# coefficients obtained: Accuracy = 0.8769
                        # MCC = 0.7283
                        # AUC = 0.91


# (ctree) Model generation ------------------------------------------------
library(party)
library(partykit)
tree_model3 <- ctree(Survived ~ ., data = train)

plot(tree_model3, main = "Tree using ctree") # auto pruned by significance test


## model evaluation --------------------------------------------------------
test$predictions_3 <- predict(tree_model3, newdata = test, type = "response")

# Confusion matrix
conf_matrix3 <- confusionMatrix(test$predictions_3, test$Survived)
print(conf_matrix3)

conf_matrix_table3 <- as.data.frame(conf_matrix3$table)
colnames(conf_matrix_table3) <- c("Predicted", "Actual", "Frequency")

ggplot(conf_matrix_table3, aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = Frequency), color = "black", size = 5) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix (Decision Tree using tree pack)",
    x = "Actual Class",
    y = "Predicted Class"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# Compute Matthews Correlation Coefficient (MCC)
predictions_3 <- as.numeric(test$predictions_3)
actuals <- as.numeric(test$Survived)
MCC <- mcc(preds = predictions_3, actuals = actuals)
print(paste("Matthews Correlation Coefficient (MCC):", MCC))

# ROC-AUC
roc_curve3 <- roc(test$Survived, as.numeric(test$predictions_3))
auc_value3 <- auc(roc_curve3)
plot(roc_curve3, col = "blue", main = paste("ROC-AUC Curve (Decision Tree-ctree):", round(auc_value3, 2)))

# COMMENT: Accuracy = 0.8731
          # MCC: 0.7283
          # AUC: 0.86
# the ctree uses the algorithm of Conditional Inference Trees (based on permutation tests).
# and independence test as splitting criterion


# INTERPRETATION AND RECAP OF RESULTS ----------------------------------------------------------
# All decision tree give good results
# (chat) it may suffer from overfitting and it's less interpretable than the logistic 
# we may analyze the influence of certain variables more than others
# for the last tree non sono stato capace di fare la Varimp ma manco chat pare sia in grado
