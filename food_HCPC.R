library(factoextra)
library(dplyr)
library(FactoMineR)
library(factoextra)

Food_Production <- read.csv("Food_Production.csv")
Food_Production <- as.data.frame(Food_Production)

Food_Production <- Food_Production %>%
  rename(food_product = 'Food.product',
         land_use_change = 'Land.use.change',
         animal_feed = 'Animal.Feed',
  )


scaled_data <- Food_Production
scaled_data[, -1] <- scale(Food_Production[, -1])
scaled_data <- scaled_data[, 2:8]

##################################
# SALINI CODE

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

# Try with 4 clusters (after trying the table method we see that Ward needs only 2) 
average <- cutree(h1, k=4)
complete<- cutree(h2, k=4)
ward<- cutree(h3, k=2)
table(average,complete)
table(average, ward)
table(complete, ward)
table(average, complete)
dev.off()

# Visualize all three plots together
opar <- par(mfrow = c(1, 3))   
plot(h1, main="average linkage")
plot(h2, main="complete linkage")
plot(h3, main="Ward linkage")
dev.off()

# Explore solutions
h1cluster <- cutree(h1, k=4)
h2cluster <- cutree(h2, k=4)
h3cluster <- cutree(h3, k=2)
plot(scaled_data[,c(1,2,3,4)], col=h1cluster, main="average linkage")
plot(scaled_data[,c(1,2,3,4)], col=h2cluster, main="complete linkage")
plot(scaled_data[,c(1,2,3,4)], col=h3cluster, main="ward linkage")

table(h1cluster)
table(h2cluster)
table(h3cluster)

# Group means
medie1<-aggregate(scaled_data, list(h1cluster), mean)
t(round(medie, 2))
medie2<-aggregate(scale(scaled_data), list(h2cluster), mean)
t(round(mediez, 2))
medie3<-aggregate(scale(scaled_data), list(h3cluster), mean)
t(round(mediez, 2))

"The first clusters includes the majority of the data, while the others have very few.
The hierarchical method based on the Ward linkage looks more informative, but the number
of clusters obtained (that we can see from the table) is only 2: increasing it to three, 
only one value is assigned to the additional cluster."

##########################################


pca_res <- prcomp(scaled_data[, 2:8], scale. = TRUE)

# Visualize variables only
fviz_pca_var(
  pca_res,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)

summary(pca_res)
fviz_pca_ind(pca_res)

# hierarchical clustering
<<<<<<< Updated upstream:food_HCPC.R
Food_Production <- Food_Production[sapply(Food_Production, is.numeric)]
pca_res <- PCA(Food_Production[, 2:8], graph = FALSE) #NA changed with the mean
res.hcpc <- HCPC(pca_res, graph = FALSE)
=======
res.hcpc <- HCPC(scaled_data, graph = FALSE)
>>>>>>> Stashed changes:food_code.R

# Number of clusters chosen
res.hcpc$call$t$nb.clust

# Visualize dendogram
plot(res.hcpc, choice = "tree")

# Visualize clusters in factor map
plot(res.hcpc, choice = "map")

<<<<<<< Updated upstream:food_HCPC.R
centroids <- res.hcpc$desc.var$quanti
print(centroids)
=======
# Centri dei cluster (medie delle variabili per ciascun cluster)
centroids <- res.hcpc$desc.var$quanti
print(centroids)

>>>>>>> Stashed changes:food_code.R
