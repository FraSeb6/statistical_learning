# Caricare le librerie
library(caret)
library(dplyr)
library(ggplot2)
library(pROC)

# Leggere il dataset
df <- read.csv("bankmarketing/bank.csv", sep = ';')

# Convertire variabili binarie (yes/no -> 1/0)
df <- df %>%
  mutate(
    default = ifelse(default == "yes", 1, 0),
    housing = ifelse(housing == "yes", 1, 0),
    loan = ifelse(loan == "yes", 1, 0),
    y = ifelse(y == "yes", 1, 0),
    contacted_before = ifelse(pdays > -1, 1, 0)
  ) %>%
  select(-pdays)

# One-hot encoding per variabili categoriche
dummies <- dummyVars("~ .", data = df, fullRank = TRUE)
df <- predict(dummies, df) %>% as.data.frame()

# Normalizzazione delle variabili numeriche
numeric_cols <- c("age", "balance", "duration", "campaign", "previous")
df[numeric_cols] <- scale(df[numeric_cols])

# Dividere i dati in train (80%) e test (20%)
set.seed(42)
trainIndex <- createDataPartition(df$y, p = 0.8, list = FALSE)
train <- df[trainIndex, ]
test <- df[-trainIndex, ]

# Creare il modello logistico
logistic_model <- glm(y ~ ., data = train, family = binomial())

# Mostrare il sommario del modello
summary(logistic_model)

# Predizioni sul test set
test$predicted_prob <- predict(logistic_model, newdata = test, type = "response")
test$predicted_class <- ifelse(test$predicted_prob > 0.5, 1, 0)

# Matrice di confusione
conf_matrix <- table(Predicted = test$predicted_class, Actual = test$y)
print(conf_matrix)

# Calcolo di sensibilità, specificità e accuratezza
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[, 1])
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

cat("Sensibilità:", sensitivity, "\n")
cat("Specificità:", specificity, "\n")
cat("Accuratezza:", accuracy, "\n")

# Curva ROC e AUC
roc_curve <- roc(test$y, test$predicted_prob)
auc_value <- auc(roc_curve)
plot(roc_curve, col = "blue", main = paste("Curva ROC - AUC:", round(auc_value, 2)))

# Grafico della distribuzione di y rispetto alla durata della chiamata
ggplot(test, aes(x = duration, fill = factor(y))) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(title = "Distribuzione della durata della chiamata rispetto a y",
       x = "Durata della chiamata", y = "Frequenza") +
  scale_fill_manual(values = c("red", "green"), name = "y") +
  theme_minimal()