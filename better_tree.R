library(dplyr)
library(caret)
library(ggplot2)
library(car)
library(mltools)
library(pROC)

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

# In order not to have to standardize we just don't consider the outliers for Fare
max(titanic$Fare, na.rm = TRUE)

titanic <- titanic[!is.na(titanic$Fare) & round(titanic$Fare, 3) != 512.329, ]

# Impute missing values ---------------------------------------------
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


# model generation --------------------------------------------------------
train$Survived <- factor(train$Survived, levels = c(0, 1), labels = c("Not Survived", "Survived"))

# CROSS-VALIDATION?
# # Suddividi il dataset in 10 fold
# cv_splits <- vfold_cv(train, v = 10)
# 
# # Inizializza un vettore per memorizzare le accuratezze di ciascun fold
# accuracies <- numeric(length = length(cv_splits$splits))
# 
# # Ciclo per allenare e testare il modello su ciascun fold
# for (i in 1:length(cv_splits$splits)) {
#   train_fold <- analysis(cv_splits$splits[[i]])
#   test_fold <- assessment(cv_splits$splits[[i]])
#   
#   # Allena il modello sull'attuale fold
#   model <- rpart(Survived ~ ., data = train_fold)
#   
#   # Fai delle predizioni
#   predictions <- predict(model, test_fold, type = "class")
#   
#   # Calcola l'accuratezza per il fold corrente
#   accuracies[i] <- mean(predictions == test_fold$Survived)
# }
# # Calcola e stampa l'accuratezza media
# average_accuracy <- mean(accuracies)
# print(paste("Average accuracy over all folds:", round(average_accuracy, 4)))


tree_model<-rpart(Survived ~ ., train)
printcp(tree_model)

plotcp(tree_model)
rpart.plot(tree_model)

# Variance Importance
print(tree_model$variable.importance)
barplot(
  tree_model$variable.importance,
  main = "Variable Importance",
  ylab = "Importance Score",
  col = "lightblue",
  las = 1
)

# Model evaluation --------------------------------------------------------
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
plot(roc_curve, col = "blue", main = paste("ROC - AUC Curve (Decision Tree):", round(auc_value, 2)))
