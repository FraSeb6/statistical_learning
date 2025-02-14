# Installare i pacchetti (se non già installati)
install.packages("caret")
install.packages("dplyr")

# Caricare le librerie
library(caret)
library(dplyr)
library(tidyverse)



df <- read.csv("bankmarketing/bank.csv", sep = ';')
df[df == "unknown"] <- NA
df[df == "-1"] <- NA

# Convertire la variabile target "y" in numerica (0 = no, 1 = yes)
df$y <- ifelse(df$y == "yes", 1, 0)



# Identificare le colonne categoriche
categorical_cols <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome")

# Creare le variabili dummy (One-Hot Encoding)
dummy_model <- dummyVars(~ ., data=df, fullRank=TRUE)  # fullRank=TRUE per evitare collinearità
df_encoded <- predict(dummy_model, newdata=df)

# Convertire in dataframe
df_encoded <- as.data.frame(df_encoded)

# Visualizzare le prime righe
head(df_encoded)

