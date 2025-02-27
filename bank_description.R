library(ggplot2)
library(dplyr)

# Load the dataset
df <- read.csv("bankmarketing/bank.csv", sep = ';')
str(df)

# Data Description --------------------------------------------------------

par(mfrow = c(2,4))

## Target distribution --------------------------------------------------------
target_dist <- ggplot(df, aes(x = y)) +
  geom_bar(aes(fill = y)) +
  geom_text(aes(y = ..count.. - 200, 
                label = paste0(round(prop.table(..count..), 4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) + 
  labs(title = "Target y distribution",
       x = "Has the customer subscribed the plan?",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

target_dist
# COMMENT: the distribution of the target shows us that there is disproportion between the two binary values,
# we will take this in consideration estimating models, trying to find ways to adjust these imbalances

## Categorical variables distribution --------------------------------------------------------
#Let's analyze the variables that we believe could be the most effective at estimating the target y

# Relationship status
relationship_dist <- ggplot(df, aes(x = marital)) +
  geom_bar(aes(fill = y)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) + 
  labs(title = "Relationship distribution",
       x = "Customer relationship status",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()
  

relationship_dist
#COMMENT: shows us that a big percentage of people in the dataset are married

# Loan
loan_dist <- ggplot(df, aes(x = factor(loan))) +
  geom_bar(aes(fill = y)) +  # Separate bars by 'y' with dodge
  geom_text(aes(y = ..count.. - 200, 
                label = paste0(round(prop.table(..count..), 4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(0.8), 
            size = 3) + 
  labs(title = "Loan distribution",
       x = "Has the customer a personal loan?",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

loan_dist

# Outcome of the previous campaign
poutcome_dist <- ggplot(df, aes(x = poutcome)) +
  geom_bar(aes(fill = y)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) + 
  labs(title = "Outcome of previous campaign distribution",
       x = "Outcome of previous campaign ",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

poutcome_dist

# Job
job_dist <- ggplot(df, aes(x = job)) +
  geom_bar(aes(fill = y)) +
  geom_text(aes(y = ..count.. - 200, 
                label = paste0(round(prop.table(..count..), 4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) + 
  labs(title = "Job distribution",
       x = "Customer's job",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

job_dist


## Numerical variables distribution --------------------------------------------------------
#Let's analyze the variables that we believe could be the most effective at estimating the target y

# Duration
duration_dist <- ggplot(df, aes(x = duration, fill = y)) +
  geom_density(aes(y = ..density..), alpha = 0.5) +
  labs(title = "Distribution of y (density) with respect to the last contact duration ",
       x = "Last contact duration", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() 


duration_dist

# Age
age_dist <- ggplot(df, aes(x = age, fill = y)) + 
  geom_density(aes(y = ..density..), alpha = 0.5) +
  labs(title = "Distribution of y (density) with respect to the age of the customer ",
       x = "age of costumer", y = "Density") +
  theme_minimal()

age_dist

# Balance
balance_dist <- ggplot(df, aes(x = balance, fill = y)) + 
  geom_density(aes(y = ..density..), alpha = 0.5) +
  labs(title = "Distribution of y (density) with respect to the balance of the customer ",
       x = "Balance of customer", y = "Density") +
  xlim(0, 10000) +
  theme_minimal()

balance_dist