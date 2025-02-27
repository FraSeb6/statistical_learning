library(ggplot2)
library(dplyr)

# Load the dataset
df <- read.csv("bankmarketing/bank.csv", sep = ';')
str(df)
head(df)

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
  theme(plot.title = element_text(hjust = 0.5))

target_dist

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
  theme(plot.title = element_text(hjust = 0.5))

relationship_dist

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
  theme(plot.title = element_text(hjust = 0.5))

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
  theme(plot.title = element_text(hjust = 0.5))

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
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

job_dist

df_normalized_cat <- df %>%
  group_by(y, job) %>%
  tally() %>%
  group_by(y) %>%
  mutate(percentage = n / sum(n) * 100)

# # Plot normalized stacked bar chart !!???
# job_dist_stacked <- ggplot(df_normalized_cat, aes(x = job, y = percentage, fill = y)) +
#   geom_bar(stat = "identity") +  # Create stacked bar chart
#   geom_text(aes(label = paste0(round(percentage, 2), '%')),
#             position = position_stack(vjust = 0.5),
#             size = 3) +  # Place text in the middle of the stack
#   labs(title = "Normalized Job distribution by Y", x = "Customer's job", y = "Percentage") +
#   theme(plot.title = element_text(hjust = 0.5),
#         axis.text.x = element_text(angle = 45, hjust = 1))
# 
# job_dist_stacked


## Numerical variables distribution --------------------------------------------------------
#Let's analyze the variables that we believe could be the most effective at estimating the target y

# Duration
duration_dist <- ggplot(df, aes(x = duration, fill = y)) +
  geom_density(aes(y = ..density..), alpha = 0.5) +
  labs(title = "Distribution of y (density) with respect to the last contact duration ",
       x = "Last contact duration", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5))

duration_dist

# Age
age_dist <- ggplot(df, aes(x = age, fill = y)) + 
  geom_density(aes(y = ..density..), alpha = 0.5) +
  labs(title = "Distribution of y (density) with respect to the age of the customer ",
       x = "age of costumer", y = "Density") 

age_dist

# Balance -> to check!!
balance_dist <-  ggplot(df, aes(x = balance, fill = y)) + 
  geom_density(aes(y = ..density..), alpha = 0.5) +
  labs(title = "Distribution of y (density) with respect to the balance of the customer ",
       x = "balance of costumer", y = "Density") 

balance_dist
