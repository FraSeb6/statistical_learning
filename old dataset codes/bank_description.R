library(ggplot2)
library(dplyr)
library(patchwork)

# Load the dataset
df <- read.csv("bankmarketing/bank.csv", sep = ';')

# Data Description --------------------------------------------------------

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
  theme(plot.title = element_text(hjust = 0.5, size = 8)) +
  theme_minimal()

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
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10)) +
  theme_minimal()
  

#COMMENT: shows us that a big percentage of people in the dataset are married
# so maybe we will rebalance this disproportion considering single and divorced together
# (also because results of these two groups alone won't be as significant)

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
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10)) +
  theme_minimal()


#COMMENT: most consumer don't have a personal loan and we notice that the percentage of y=yes
# in this group is higher than for those who have a personal loan 
# meaning that, logically, those who have a loan won't have as much liquidity to invest as those who don't
# and this may generate significant results in our prediction

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
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10, margin = margin(b = 2))) +
  theme_minimal()


#COMMENT: we have too much missing info for the previous campaign outcome, 
# so we won't consider this variable

# contact communication type (contact)
contact_dist <-  ggplot(df, aes(x = contact)) +
  geom_bar(aes(fill = y)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) + 
  labs(title = "Contact communication type distribution",
       x = "Contact communication type ",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10, )) +
  theme_minimal()


# COMMENT: as for the outcome for previous campaign we won't consider this variable

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
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

job_dist

# COMMENT: Since there are a lot of categories it may be more significant to group them


## Numerical variables distribution --------------------------------------------------------
#Let's analyze the variables that we believe could be the most effective at estimating the target y

# Duration
duration_dist <- ggplot(df, aes(x = duration, fill = y)) +
  geom_density(aes(y = ..density..), alpha = 0.5) +
  labs(title = "Distribution of y with respect to duration ",
       x = "Last contact duration", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 7)) +
  theme_minimal() 


# COMMENT: it seems that an higher contact duration denotes a higher chance for the subscription of the plan 

# Age
age_dist <- ggplot(df, aes(x = age, fill = y)) + 
  geom_density(aes(y = ..density..), alpha = 0.5) +
  labs(title = "Distribution of y  with respect to the age",
       x = "age of costumer", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 7)) +
  theme_minimal()


# COMMENT: it seems that this proposal attracts older people more

# Balance
balance_dist <- ggplot(df, aes(x = balance, fill = y)) + 
  geom_density(aes(y = ..density..), alpha = 0.5) +
  labs(title = "Distribution of y with respect to the customer",
       x = "Balance of customer", y = "Density") +
  xlim(0, 10000) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 7)) +
  theme_minimal()


# COMMENT: An higher average yearly balance corresponds to an higher chance of subscription

# Visualization -----------------------------------------------------------
(target_dist + relationship_dist + loan_dist) /
  (poutcome_dist + contact_dist + job_dist) /
  (duration_dist + age_dist + balance_dist)

