# Installare i pacchetti (se non già installati)
install.packages("caret")
install.packages("dplyr")

# Caricare le librerie
library(caret)


library(ggplot2)
library(ROCR)
library(nnet)
library(randomForest)
library(e1071)
library(MASS)
library(mlbench)


df <- read.csv("bankmarketing/bank.csv", sep = ';')
df[df == "unknown"] <- NA
df[df == "-1"] <- NA

# Convertire la variabile target "y" in numerica (0 = no, 1 = yes)
df$y <- ifelse(df$y == "yes", 1, 0)





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

