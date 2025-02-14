library(dplyr)

df <- read.csv("bankmarketing/bank.csv", sep = ';')
df[df == "unknown"] <- NA
