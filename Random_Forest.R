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

# Set seed for riproducibility 
set.seed(42)

# Define the parameter for the traning
train_control <- trainControl(method = "cv", number = 10, search = "grid")

# Random Forest Model Training
rf_model_caret <- train(y ~ ., 
                        data = train, 
                        method = "rf", 
                        trControl = train_control, 
                        tuneLength = 5)  # Puoi regolare il numero di parametri da provare

# Visualizza il modello allenato
print(rf_model_caret)

# Visualizza i parametri ottimali trovati durante la validazione incrociata
rf_model_caret$bestTune

# Fare predizioni sui dati di test
predictions_caret <- predict(rf_model_caret, newdata = test)

# Crea la matrice di confusione per calcolare l'accuratezza
confusion_matrix_caret <- confusionMatrix(predictions_caret, test$y)
print(confusion_matrix_caret)

# Stampa l'accuratezza
cat("Accuracy: ", confusion_matrix_caret$overall["Accuracy"], "\n")

# Ottieni le probabilità per il caso positivo
probabilities_caret <- predict(rf_model_caret, newdata = test, type = "prob")[, 2]

# Crea la curva ROC
roc_curve_caret <- roc(test$y, probabilities_caret)
auc_value <- auc(roc_curve_caret)
plot(roc_curve_caret, main = paste("ROC - AUC curve:", round(auc_value,2)), col = "blue")

# Visualizza l'importanza delle variabili
rf_model_caret$finalModel$importance
