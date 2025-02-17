library(factoextra)
library(dplyr)
library(FactoMineR)
library(factoextra)

# Download dataset
Food_Production <- read.csv("Food_Production.csv")

# Rename variables
Food_Production <- Food_Production %>%
  rename(food_product = 'Food.product',
         land_use_change = 'Land.use.change',
         animal_feed = 'Animal.Feed',
  )

# Scale the data (except first column) and select variables until 8th.
scaled_data <- Food_Production
scaled_data[, -1] <- scale(Food_Production[, -1])
scaled_data <- scaled_data[, 2:8]


## HIERARCHICAL CLUSTERING
# Compare the results of hierarchical clustering based on average, complete and ward linkage methods.
# Build the dendograms for each method

#Avg linkage plot
h1<-hclust(dist(scaled_data), method="average")
plot(h1, main="average linkage")
rect.hclust(h1, 4)

# Complete linkage plot 
h2<-hclust(dist(scaled_data), method="complete")
plot(h2, main="complete linkage")
rect.hclust(h2, 4)

# Ward linkage plot
h3<-hclust(dist(scaled_data), method="ward.D2")
plot(h3, main="Ward linkage")
rect.hclust(h3, 4)

# Try with 4 clusters 
# After trying the table method we see that Ward needs only 2

average <- cutree(h1, k=4)
complete<- cutree(h2, k=4)
ward<- cutree(h3, k=4)

# Compare the linkage methods in pairs
table(average,complete)
table(average, ward)
table(complete, ward)
table(average, complete)

# Visualize all three plots together
opar <- par(mfrow = c(1, 3))   
plot(h1, main="average linkage")
plot(h2, main="complete linkage")
plot(h3, main="Ward linkage")

# Explore solutions
plot(scaled_data[,c(1,2,3,4)], col=average, main="average linkage")
plot(scaled_data[,c(1,2,3,4)], col=complete, main="complete linkage")
plot(scaled_data[,c(1,2,3,4)], col=ward, main="ward linkage")

table(average)
table(complete)
table(ward)

# Cluster means
means1<-aggregate(scaled_data, list(average), mean)
t(round(means1, 2))
means2<-aggregate(scale(scaled_data), list(complete), mean)
t(round(means2, 2))
means3<-aggregate(scale(scaled_data), list(ward), mean)
t(round(means3, 2))

"The first clusters includes the majority of the data, while the others have very few.
The hierarchical method based on the Ward linkage looks more informative, but the number
of clusters obtained (that we can see from the table) is only 2: increasing it to three, 
only one value is assigned to the additional cluster."


## PRINCIPAL COMPONENT ANALYSIS

# PCA on scaled data
pca_res <- prcomp(scaled_data[, 1:7], scale. = TRUE)

# Visualize variables only to see which contribute more to the PC variance 
fviz_pca_var(
  pca_res,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)

# How much variance is explained by each component 
summary(pca_res)
# Show the data projected on the first two PC
fviz_pca_ind(pca_res)


## HCPC
# Hierarchical clustering on Principal Components
# Try to combine PCA and hierarchical clustering to see if we get better results

# Remove non-numeric columns
Food_Production <- Food_Production[sapply(Food_Production, is.numeric)]

# PCA (most suitable method for HCPC)
pca_res <- PCA(Food_Production[, 2:8], graph = FALSE) #NA changed with the mean

# Combine PCA and hierarchical clustering
res.hcpc <- HCPC(pca_res, graph = FALSE)

# Number of clusters to choose
# We get 7 clusters
res.hcpc$call$t$nb.clust

# Visualize dendogram
plot(res.hcpc, choice = "tree")

# Visualize clusters in factor map: distribution of data wrt the PC
plot(res.hcpc, choice = "map")

# Extract centroids (mean for each cluster)
centroids <- res.hcpc$desc.var$quanti
print(centroids)

# Variable means for each cluster
# We see that clusters 2, 3, 4, 5 include only one observation: this method is not optimal.
centroids <- res.hcpc$desc.var$quanti
print(centroids)


"We started by applying the hierarchical clustering method on scaled data.
After that, we used PCA to reduce data dimensionality.
Eventually, we combined PCA and clustering through the HCPC, to obtain a robust solution."
