library(ggplot2)
library(dplyr)
library(patchwork)

# Data -------------------------------------------------------

# Load the datasets
titanic <- read.csv("titanic_combined.csv")
titanic <- titanic[, !names(titanic) %in% c("PassengerId", "Name", "Ticket", "Cabin")]
titanic[titanic == ""] <- NA

titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Survived <- factor(titanic$Survived, levels = c(0, 1), labels = c("No", "Yes"))

# Round ages < 1 to 1
titanic$Age[titanic$Age < 1] <- 1
titanic$Age <- round(titanic$Age)

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Impute Age with median
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)

# Impute Embarked with mode
titanic$Embarked[is.na(titanic$Embarked)] <- get_mode(titanic$Embarked)

# Impute Fare with median
titanic$Fare[is.na(titanic$Fare)] <- median(titanic$Fare, na.rm = TRUE)


# Target distribution --------------------------------------------------------

target_dist <- ggplot(titanic, aes(x = Survived, fill = Survived)) +
  geom_bar(color = "black") +
  geom_text(aes(y = ..count.., 
                label = paste0(round(..count.. / sum(..count..) * 100, 1), "%")), 
            stat = 'count', 
            position = position_stack(vjust = 0.5), 
            size = 3) +
  labs(title = "Survived Distribution", x = "Survived", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# the target distribution showing us that an higher percentage of people on the Titanic
# did not survive, but not a big enough unbalance to ask us to rebalance the levels

# Categorical Variables -------------------------------------------------------------------------

# Pclass distribution
class_counts <- titanic %>%
  group_by(Pclass, Survived) %>%
  summarise(count = n()) %>%
  group_by(Pclass) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

class_dist <- ggplot(class_counts, aes(x = Pclass, y = count, fill = Survived)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(y = count, 
                label = paste0(percentage, "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Class Distribution by Survival", x = "Pclass", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#pclass: A proxy for socio-economic status (SES)
# 1st = Upper
# 2nd = Middle
# 3rd = Lower
# COMMENT: we can already notice an higher percentage of people survived from the upper group

# Sex distribution
sex_counts <- titanic %>%
  group_by(Sex, Survived) %>%
  summarise(count = n()) %>%
  group_by(Sex) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

sex_dist <- ggplot(sex_counts, aes(x = Sex, y = count, fill = Survived)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(y = count, 
                label = paste0(percentage, "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Sex Distribution by Survival", x = "Sex", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#COMMENT: As we could expect, a much higher percentage of female saved

# Embarked distribution
embarked_counts <- titanic %>%
  group_by(Embarked, Survived) %>%
  summarise(count = n()) %>%
  group_by(Embarked) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

embarked_dist <- ggplot(embarked_counts, aes(x = Embarked, y = count, fill = Survived)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(y = count, 
                label = paste0(percentage, "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Embarked Distribution by Survival", x = "Embarked", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Not a big difference in place of embarkment, as we could expect


# Numerical Variables -------------------------------------------------------------------------

# SibSp distribution
sibsp_counts <- titanic %>%
  group_by(SibSp, Survived) %>%
  summarise(count = n()) %>%
  group_by(SibSp) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

sibsp_dist <- ggplot(sibsp_counts, aes(x = SibSp, y = count, fill = Survived)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(y = count, 
                label = ifelse(count > 10, paste0(percentage, "%"), "")),  # Only show percentage if count > 10
            position = position_stack(vjust = 0.5), size = 3) +
  scale_x_continuous(breaks = 0:8) +
  labs(title = "SibSp Distribution by Survival", x = "Siblings/Spouses (SibSp)", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# COMMENT: In general, stopping our interpretation just to "classes" 
# with none or one sibling or spouse, we notice that an higher percentage of people
# saved within those that had one of them, but the difference in numbers is too high to interpret them correctly

# Parch distribution
parch_counts <- titanic %>%
  group_by(Parch, Survived) %>%
  summarise(count = n()) %>%
  group_by(Parch) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

parch_dist <- ggplot(parch_counts, aes(x = Parch, y = count, fill = Survived)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(y = count, 
                label = ifelse(count > 10, paste0(percentage, "%"), "")),  # Only show percentage if count > 10
            position = position_stack(vjust = 0.5), size = 3) +
  scale_x_continuous(breaks = 0:9) +
  labs(title = "Parch Distribution by Survival", x = "Parents/Children (Parch)", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# COMMENT: similar to the SibSp Distribution


# Age distribution
age_dist <- ggplot(titanic, aes(x = Age, fill = Survived)) + 
  geom_density(aes(y = ..density..), alpha = 0.5) +
  labs(title = "Age distribution by Survival",
       x = "Age", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 7)) +
  theme_minimal()

# COMMENT: We can start noticing that children and adults had different survival rates 


# Fare distribution
fare_dist <- ggplot(titanic, aes(x = Fare, fill = Survived)) + 
  geom_density(aes(y = ..density..), alpha = 0.5) +
  labs(title = "Fare Distribution by Survival",
       x = "Fare", y = "Density") +
  xlim(0, 300) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 7)) +
  theme_minimal()

# COMMENT: Again a pretty similar conclusion as the one we had from PClass
# people who payed more for their ticket have an higher survival rate, 
# but the relation might not be that significant 

# Visualize ---------------------------------------------------------------

# Combine all plots using patchwork layout

# Target distribution
target_dist

# Categorical variables distribution
class_dist +
  sex_dist +
  embarked_dist +
  plot_layout(ncol = 3)

# Numerical variables distribution
sibsp_dist +
  parch_dist +
  age_dist +
  fare_dist +
  plot_layout(ncol = 2)
