# Load necessary libraries
library(dplyr)
library(caret)
library(ggplot2)
library(car)
library(mltools)

# Load the datasets
train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)

# Extract first letter of cabin as a categorical feature
train$Cabin_Initial <- substr(train$Cabin, 1, 1)
test$Cabin_Initial <- substr(test$Cabin, 1, 1)

# Convert categorical variables to factors
train$Pclass <- as.factor(train$Pclass)
test$Pclass <- as.factor(test$Pclass)
train$Sex <- as.factor(train$Sex)
test$Sex <- as.factor(test$Sex)
train$Embarked <- as.factor(train$Embarked)
test$Embarked <- as.factor(test$Embarked)
train$Cabin_Initial <- as.factor(train$Cabin_Initial)
test$Cabin_Initial <- as.factor(test$Cabin_Initial)
train$Survived <- as.factor(train$Survived) # Ensure Survived is a factor for classification

# Select relevant features for logistic regression (excluding HasCabin)
train <- train %>% select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked, Cabin_Initial)
test <- test %>% select(Pclass, Sex, Age, SibSp, Parch, Fare, Embarked, Cabin_Initial)

# Remove rows with missing values
train <- na.omit(train)
test <- na.omit(test)

# Compute correlation matrix for numeric variables
numeric_vars <- train %>% select(Age, SibSp, Parch, Fare) %>% cor()
print("Correlation matrix:")
print(numeric_vars)

# Remove highly correlated variables (correlation > 0.8)
high_corr_features <- findCorrelation(numeric_vars, cutoff = 0.8, names = TRUE)
print("Highly correlated features to remove:")
print(high_corr_features)
train <- train %>% select(-one_of(high_corr_features))
test <- test %>% select(-one_of(high_corr_features))

# Check for multicollinearity using Variance Inflation Factor (VIF)
vif_model <- glm(Survived ~ ., data = train, family = binomial)
vif_values <- vif(vif_model)
print("Variance Inflation Factor (VIF) values:")
print(vif_values)

# Remove features with high VIF (> 5)
high_vif_features <- names(vif_values[vif_values > 5])
train <- train %>% select(-one_of(high_vif_features))
test <- test %>% select(-one_of(high_vif_features))

# Train logistic regression model using caret for classification
set.seed(123)
logit_model <- train(Survived ~ ., data = train, method = "glm", family = binomial,
                     trControl = trainControl(method = "cv", number = 10), 
                     metric = "Accuracy")

# Summary of the model
summary(logit_model$finalModel)

# Predict on test dataset
test$Survival_Probability <- predict(logit_model, newdata = test, type = "prob")[,2]

# Convert probabilities to binary outcomes (threshold = 0.5)
test$Survived <- ifelse(test$Survival_Probability > 0.5, 1, 0)

# Evaluate model performance
conf_matrix <- confusionMatrix(predict(logit_model, newdata = train), train$Survived)
print(conf_matrix)

# Compute Matthews Correlation Coefficient (MCC)
predictions <- as.numeric(predict(logit_model, newdata = train))
actuals <- as.numeric(train$Survived)
MCC <- mcc(preds = predictions, actuals = actuals)
print(paste("Matthews Correlation Coefficient (MCC):", MCC))

# View prediction results
head(test$Survived)
