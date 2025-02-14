library(factoextra)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(rattle)
library(cluster)

Food_Production <- read.csv("Food_Production.csv")
Food_Production <- as.data.frame(Food_Production)

Food_Production <- Food_Production %>%
  rename(food_product = 'Food.product',
         land_use_change = 'Land.use.change',
         animal_feed = 'Animal.Feed',
  )

# Scale data
scaled_data <- Food_Production
scaled_data[, -1] <- scale(Food_Production[, -1])
scaled_data <- scaled_data[, 2:8]

"We decided to try to cluster the data using the k-median algorithm,
so to reduce the influence of the outlier 34 on the model."

set.seed(123)
k.median.fit <- pam(scaled_data, 4, metric = "manhattan")  # metric = "manhattan" per k-median
str(k.median.fit)
scaled_data$cluster <- k.median.fit$clustering # assign clusters to the data
table(k.median.fit$clustering, scaled_data$cluster)

# Plot the clusters
clusplot(scaled_data, k.median.fit$clustering, 
         main='2D representation of the k-median Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

"In this case, the decision of having 4 clusters appear the one that fit best with the data.
Nevertheless, the k-median does not seem to be a good method to cluster our data, because
clusters 1, 2, 3 are very close to each other."
