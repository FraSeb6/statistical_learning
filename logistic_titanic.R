# Load necessary libraries
library(dplyr)

# Load the datasets
train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)

# Count missing values per column
na_counts_train <- colSums(is.na(train))
na_counts_test <- colSums(is.na(test))

# Count missing values per row
na_rows_train <- rowSums(is.na(train))
na_rows_test <- rowSums(is.na(test))

# Print missing values count
print("Missing values per column in training set:")
print(na_counts_train)
print("Missing values per column in test set:")
print(na_counts_test)
print("Missing values per row in training set:")
print(table(na_rows_train))
print("Missing values per row in test set:")
print(table(na_rows_test))
