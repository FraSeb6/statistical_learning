# Caricare le librerie
library(caret)
library(dplyr)
library(tidyverse)


df <- read.csv("bankmarketing/bank.csv", sep = ';')

df <- df %>%
  mutate(
    # Binary variables (convert yes/no to 1/0)
    default = ifelse(default == "yes", 1, 0),
    housing = ifelse(housing == "yes", 1, 0),
    loan = ifelse(loan == "yes", 1, 0),
    y = ifelse(y == "yes", 1, 0),
    
    # Transform pdays into two variables
    contacted_before = ifelse(pdays > -1, 1, 0),
  ) %>%
  select(-pdays) # Remove original pdays column

# One-hot encoding for categorical variables
df <- dummyVars("~ .", data = df, fullRank = TRUE) %>% 
  predict(df) %>%
  as.data.frame()

# Normalize numeric columns (age, balance, duration, campaign, previous)
numeric_cols <- c("age", "balance", "duration", "campaign", "previous")
df[numeric_cols] <- scale(df[numeric_cols])

# Creare il modello di regressione logistica
logistic_model <- glm(y ~ ., data = df, family = binomial())

# Mostrare il sommario del modello
summary(logistic_model)

