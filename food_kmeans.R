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

# Find optimal number of clusters
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(123)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(scaled_data, nc=10)

"It looks from the plot that 4 is the optimal number of cluster"

set.seed(123)
k.means.fit <- kmeans(scaled_data, 4)
str(k.means.fit)
scaled_data$cluster <- k.means.fit$cluster
table(k.means.fit$cluster, scaled_data$cluster)

"From the table of the k-means fit it, we find out that the second cluster has only three observations.
It is probably more convenient to try with three clusters."

clusplot(scaled_data, k.means.fit$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

"We can see from the plot that the second cluster that is introduced when the number is set to 4,
is probably due to the presence of the outlier 34.
We can try to apply the k-median method, or re-try the k-means after dropping the outlier."


