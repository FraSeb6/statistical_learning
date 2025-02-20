# Installare i pacchetti (se non già installati)
install.packages("caret")
install.packages("dplyr")

# Caricare le librerie
library(caret)
<<<<<<< Updated upstream


library(ggplot2)
library(ROCR)
library(nnet)
library(randomForest)
library(e1071)
library(MASS)
library(mlbench)

=======
library(dplyr)
library(ggplot2)
library(pROC)
>>>>>>> Stashed changes

# Leggere il dataset
df <- read.csv("bankmarketing/bank.csv", sep = ';')
df[df == "unknown"] <- NA
df[df == "-1"] <- NA

<<<<<<< Updated upstream
# Convertire la variabile target "y" in numerica (0 = no, 1 = yes)
df$y <- ifelse(df$y == "yes", 1, 0)



=======
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
>>>>>>> Stashed changes


set.seed(123)
train_index <- createDataPartition(credit_data$Class, p = 0.6, list = FALSE)
train_data <- credit_data[train_index, ]
test_data  <- credit_data[-train_index, ]




# Identificare le colonne categoriche
categorical_cols <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome")

# Creare le variabili dummy (One-Hot Encoding)
dummy_model <- dummyVars(~ ., data=df, fullRank=TRUE)  # fullRank=TRUE per evitare collinearità
df_encoded <- predict(dummy_model, newdata=df)

# Convertire in dataframe
df_encoded <- as.data.frame(df_encoded)

# Visualizzare le prime righe
head(df_encoded)







# Installare e caricare il pacchetto necessario
install.packages("caTools")
library(caTools)

# Creare un seed per riproducibilità
set.seed(42)

# Creare l'indice di suddivisione (80% training, 20% test)
split <- sample.split(df_encoded$y, SplitRatio = 0.8)

# Creare i dataset di training e test
train_set <- subset(df_encoded, split == TRUE)
test_set <- subset(df_encoded, split == FALSE)

# Controllare le dimensioni
dim(train_set)
dim(test_set)






# Installare il pacchetto necessario
install.packages("glmnet")

# Caricare la libreria
library(glmnet)

# Addestrare il modello
model_logistic <- glm(y ~ ., data=train_set, family=binomial)

# Visualizzare il riepilogo del modello
summary(model_logistic)





# Predire la probabilità della classe 1 (Yes)
prob_pred <- predict(model_logistic, newdata=test_set, type="response")

# Convertire le probabilità in classi (soglia 0.5)
y_pred <- ifelse(prob_pred > 0.5, 1, 0)

# Convertire in fattori per confronto con i dati reali
y_pred <- factor(y_pred, levels = c(0,1))
y_real <- factor(test_set$y, levels = c(0,1))








# Installare il pacchetto per metriche di classificazione
install.packages("caret")
library(caret)

# Calcolare la matrice di confusione
conf_matrix <- confusionMatrix(y_pred, y_real)

# Stampare i risultati
print(conf_matrix)







# Installare il pacchetto (se non già installato)
install.packages("DMwR")

# Caricare la libreria
library(DMwR)

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

