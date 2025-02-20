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

# Compare clusters
aggregate(scaled_data,by=list(k.means.fit$cluster),FUN=mean)


"From the table of the k-means fit it, we find out that the second cluster has only three observations.
It is probably more convenient to try with three clusters."

clusplot(scaled_data, k.means.fit$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

"We can see from the plot that the second cluster that is introduced when the number is set to 4,
is probably due to the presence of the outlier 34.
We can try to apply the k-median method, or re-try the k-means after dropping the outlier."


# Check that 34 is actually an outlier

# Create and save the boxplot
boxplot_info <- boxplot(scaled_data, 
                        main = "Boxplot con Outlier Etichettati", 
                        col = "lightblue", 
                        pch = 20)  # pch=20 per rendere i punti più visibili

# I want the labels for outliers:
for (i in 1:ncol(scaled_data)) {
  variable_values <- scaled_data[, i] 
  outliers <- boxplot_info$out[boxplot_info$group == i]  # Select only outliers for each variable
  outlier_indices <- which(variable_values %in% outliers)  # Obtain indexes
  
  # Add labels to the outliers
  if (length(outliers) > 0) {
    text(rep(i, length(outliers)), outliers, 
         labels = outlier_indices, pos = 3, col = "red", cex = 0.8)
  }
}

" From the boxplot I can see that 34 is always present as outlier in every variable, 
except for the last one which does not present outliers."

# Drop the outlier
scaled_data <- scaled_data[-34, ]  

# Try again the k-means algorithm 
set.seed(123)
k.means.fit.woutlier <- kmeans(scaled_data, 3)
str(k.means.fit.woutlier)
scaled_data$cluster <- k.means.fit.woutlier$cluster
table(k.means.fit.woutlier$cluster, scaled_data$cluster)

clusplot(scaled_data, k.means.fit.woutlier$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

"The clusters look different from before, but now it appears that they fit the data better."