library(dplyr)
library(gplots)
library(factoextra)
library(FactoMineR)
library(factoextra)
library(rattle)
library(cluster)
library(caret)
library(plotly)
library(dbscan)


Food_Production <- read.csv("Food_Production.csv")

Food_Production <- Food_Production %>%
  rename(food_product = 'Food.product',
         land_use_change = 'Land.use.change',
         animal_feed = 'Animal.Feed',
         Eutrophication_kcal = 'Eutrophying.emissions.per.1000kcal..gPO.eq.per.1000kcal.',
         Eutrophication_protein = 'Eutrophying.emissions.per.100g.protein..gPO.eq.per.100.grams.protein.',
         Eutrophication_kilogram = 'Eutrophying.emissions.per.kilogram..gPO.eq.per.kilogram.',
         Liters_kcal = 'Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal.',
         Liters_protein = 'Freshwater.withdrawals.per.100g.protein..liters.per.100g.protein.',
         Liters_kilogram = 'Freshwater.withdrawals.per.kilogram..liters.per.kilogram.',
         Gas_emissions_kcal = 'Greenhouse.gas.emissions.per.1000kcal..kgCO.eq.per.1000kcal.',
         Gas_emissions_protein = 'Greenhouse.gas.emissions.per.100g.protein..kgCO.eq.per.100g.protein.',
         Land_use_kcal = 'Land.use.per.1000kcal..m..per.1000kcal.',
         Land_use_kilogram = 'Land.use.per.kilogram..m..per.kilogram.',
         Land_use_protein = 'Land.use.per.100g.protein..m..per.100g.protein.',
         Scarcity_liters_kilogram = 'Scarcity.weighted.water.use.per.kilogram..liters.per.kilogram.',
         Scarcity_liters_protein = 'Scarcity.weighted.water.use.per.100g.protein..liters.per.100g.protein.',
         Scarcity_liters_kcal = 'Scarcity.weighted.water.use.per.1000kcal..liters.per.1000.kilocalories.'
         
  )

Food_Production[-1] <- lapply(Food_Production[-1], function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  }
  return(x)
})

---------------------------------------------------------------------------------

## DESCRIPTIVE STATISTICS

summary(Food_Production)
"We can see that our dataset is very dishomogeneous on the ranges of the variables.
We decide to visualize the variables one by one with histograms and boxplots, to 
verify which columns we should convert into logaritms."

# Histograms, boxplot: check variables' distributions
par(mfrow=c(6, 4), mar=c(2, 2, 2, 2), oma=c(0, 0, 3, 0))
for(i in 2:23) {
  hist(Food_Production[, i], 
       main=names(Food_Production)[i], 
       col="lightblue")}
mtext("Variables original distribution", outer=TRUE, cex=1.5, font=2)
dev.off()

boxplot(Food_Production[, -1], 
        main="Distribuzione delle Variabili", 
        las=2, 
        col="lightblue")

# Density plot: help visualization
plot(density(Food_Production[,2], na.rm=TRUE), 
     col=rainbow(23)[1], 
     lwd=2, 
     main="Original dataset density plot")
for (i in 3:ncol(Food_Production)) {
  lines(density(Food_Production[,i], na.rm=TRUE), 
        col=rainbow(23)[i], 
        lwd=2)}
legend("topright", legend=names(Food_Production)[2:23], col=rainbow(23), lwd=2, cex=0.6)

"The distribution of some variables is very skewed to the right: converting them
to logarithms could change this asymmetry. We cannot keep the dataset like this if 
we want to compare variables.
We decide to convert variables 13:15 and 21:23 (those that reach 1,000 as maximum
value) of our dataset into their logarithmic form to adjust the skeweness."

# Convert the most skewed variables to logarithms
Foodproduction_log <- Food_Production
Foodproduction_log[, 21:23] <- log1p(Food_Production[, 21:23])
Foodproduction_log[, 13:15] <- log1p(Food_Production[, 13:15])

plot(density(Foodproduction_log[,2], na.rm=TRUE), 
     col=rainbow(23)[1], 
     lwd=2, 
     main="Modified variables density plot")
for (i in 3:ncol(Foodproduction_log)) {
  lines(density(Foodproduction_log[,i], na.rm=TRUE), 
        col=rainbow(23)[i], 
        lwd=2)}
legend("topright", legend=names(Food_Production)[2:23], col=rainbow(23), lwd=2, cex=0.6)

"We see from the density plot that we have many different peaks: it may be
a good sign for the clustering process that we are going to explore, since it is
likely that our data is naturally split into different groups."

# Check skewness: repeat visualization
par(mfrow=c(6,4), mar=c(2, 2, 2, 2), oma=c(0, 0, 3, 0))
for(i in 2:23) {
  hist(Foodproduction_log[, i], 
       main=names(Foodproduction_log)[i], 
       col="lightblue")}
mtext("Modified variables distribution (logs)", outer=TRUE, cex=1.5, font=2)
dev.off()

boxplot(Foodproduction_log[, -1], 
        main="Distribuzione delle Variabili", 
        las=2, 
        col="lightblue")

"Some variables still have very wide ranges compared to others (to the first 8 for
example, so we scale them."

# Scale data
scaled_foodproduction <- Foodproduction_log
scaled_foodproduction[, -1] <- scale(Foodproduction_log[, -1])
scaled_foodproduction <- scaled_foodproduction[, 2:23]

# Check the scaling 
plot(density(scaled_foodproduction[,2], na.rm=TRUE), 
     col=rainbow(23)[1], 
     lwd=2, 
     main="Scaled variables density plot")
for (i in 3:ncol(scaled_foodproduction)) {
  lines(density(scaled_foodproduction[,i], na.rm=TRUE), 
        col=rainbow(23)[i], 
        lwd=2)}
legend("topright", legend=names(Food_Production)[2:23], col=rainbow(23), lwd=2, cex=0.6)

par(mfrow=c(7,3), mar=c(2, 2, 2, 2), oma=c(0, 0, 3, 0))
for(i in 2:23) {
  hist(scaled_foodproduction[, i], 
       main=names(scaled_foodproduction)[i], 
       col="lightblue")}
mtext("Scaled variables distribution", outer=TRUE, cex=1.5, font=2)
dev.off()

# Verify correlation between variables to avoid multi-collinearity
corr_matrix <- cor(scaled_foodproduction)
heatmap.2(corr_matrix, 
          main="Correlation map", 
          cex.main=0.8, 
          cexRow=0.6, 
          cexCol=0.6, 
          lwd=0.5,   # line width
          trace="none", 
          sepwidth=c(0.01, 0.01), # Shortens the lines
          colsep=1:ncol(corr_matrix), 
          rowsep=1:nrow(corr_matrix),
          srtCol=45,
          srtRow=45)

"It is clear from the heatmap that scarcity-weighted water use is strongly correlated 
to the fresh water withdrawals (corr > 0.9) We can remove one of the two groups.
We decide to remove the scarcity-weighted variables."

scaled_foodproduction <- scaled_foodproduction[, -c(20:22)]

corr_matrix <- cor(scaled_foodproduction)
heatmap(corr_matrix, main="Correlation map")

# Additional check to control for variables with a variance threshold
variances <- apply(scaled_foodproduction, 2, var)
selected_vars <- names(variances[variances > 0.3])
" All 19 variables have enough variance to be considered"

--------------------------------------------------------------------------------

## PRINCIPAL COMPONENT ANALYSIS
pca_result <- prcomp(scaled_foodproduction, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x[, 1:3])  

scaled_foodproduction$PC1 <- pca_data$PC1
scaled_foodproduction$PC2 <- pca_data$PC2
scaled_foodproduction$PC3 <- pca_data$PC3

fviz_pca_var(
  pca_result,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)

"It looks like variables point mainly towards towards the positive side of the 
second dimension (which in effect explains only 13.5% of the variance in the dataset),
while they are more dispersed on the first dimension, which alone explains 44% 
of the variance."

summary(pca_result) # How much variance is explained by each component 
fviz_pca_ind(pca_result)

"It looks like that the majority of the observations are all very similar, 
while outside that we have a few observations scattered over the dimensions."


# We may try the visualization with three components:
var_coords <- as.data.frame(pca_result$rotation[,1:3]) # variables coordinates
colnames(var_coords) <- c("Dim1", "Dim2", "Dim3")

PCA_plot_3d <- plot_ly()
for (i in 1:nrow(var_coords)) {
  PCA_plot_3d <- PCA_plot_3d %>% add_trace(
    type = "scatter3d",
    mode = "lines+text",
    x = c(0, var_coords$Dim1[i]), 
    y = c(0, var_coords$Dim2[i]), 
    z = c(0, var_coords$Dim3[i]), 
    text = c("", rownames(var_coords)[i]),
    textposition = "top center",
    line = list(width = 4),
    marker = list(size = 4, color = "red")
  )
}

PCA_plot_3d <- PCA_plot_3d %>% layout(
  scene = list(
    xaxis = list(title = "PC1"),
    yaxis = list(title = "PC2"),
    zaxis = list(title = "PC3")
  )
)
PCA_plot_3d

"We see that PC2 combined with PC3 helps clarifying the dispersion, as all 
variables clearly point towards different directions.
On the first dimension (PC1) vectors points towards positive values, but still
appear scattered on both the 1st and 2nd dimensions."

--------------------------------------------------------------------------------

# Non-linear method to reduce dimensionality using two dimensions.
library(Rtsne)

# Try t-SNE method: t-distributed Stochastic Neighbor Embedding)
"T-SNE should preserve the local structure of the dataset, and therefore it should
emphasize natural clusters if we have them already in the original data.
To help visualization, we set a relatively low perplexity paramether, which clearly
separates observations even if they are close one another."

set.seed(42)
tsne_results <- Rtsne(scaled_foodproduction[,1:19], dims = 2, perplexity = 5, verbose = TRUE, max_iter = 500)
df_tsne <- data.frame(Dim1 = tsne_results$Y[,1], Dim2 = tsne_results$Y[,2])

ggplot(df_tsne, aes(x = Dim1, y = Dim2)) +
  geom_point(alpha = 0.7) +
  ggtitle("Visualization with t-SNE") +
  theme_minimal()


--------------------------------------------------------------------------------

## K-MEANS
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(123)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
# Find optimal number of clusters

wssplot(scaled_foodproduction, nc=10)

"It looks from the plot that 4 would be the optimal number of cluster."

set.seed(123)
k.means.fit <- kmeans(scaled_foodproduction, 4)
str(k.means.fit)
scaled_foodproduction$cluster <- k.means.fit$cluster
table(k.means.fit$cluster, scaled_foodproduction$cluster)


clusplot(scaled_foodproduction, k.means.fit$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

" 33 and 32 are in the same cluster, but very far from the others. Same for 36 and 34.
Even when trying to add more clusters, they do not separate, so they must be really close."

" We should check why they are so far away.
We can also see from the plot that that cluster contains the number 34, which is probably an outlier.
We can try to apply the k-median method, or re-try the k-means after dropping the outlier."

# We can also try to look at the 3D plot: use the first three principal components
set.seed(123)  
kmeans_result <- kmeans(pca_data, centers = 4, nstart = 25)  

pca_data$Cluster_kmeans <- as.factor(kmeans_result$cluster)
pca_data$RowIndex <- rownames(scaled_foodproduction)  

plot_ly(pca_data, 
        x = ~PC1, 
        y = ~PC2, 
        z = ~PC3, 
        color = ~Cluster_kmeans, 
        text = ~paste("Row:", RowIndex),  
        colors = c("red", "blue", "green"),
        type = "scatter3d", 
        mode = "markers") %>%
  layout(title = "Cluster in PCA Space")


# Verify that rows 32-33-34-36 are actually outliers
boxplot_info <- boxplot(scaled_foodproduction, main = "Boxplot: highlight outliers", col = "lightblue")  
highlight_indices <- c(32, 33, 34, 36)
highlight_colors <- c("red", "green", "blue", "purple")  
names(highlight_colors) <- highlight_indices  # Associate the colors to the indexes

for (i in 1:ncol(scaled_foodproduction)) {
  variable_values <- scaled_foodproduction[, i] 
  outliers <- boxplot_info$out[boxplot_info$group == i]  
  outlier_indices <- which(variable_values %in% outliers)  
  
  for (idx in highlight_indices) {
    if (idx %in% outlier_indices) {
      text(i, variable_values[idx], labels = as.character(idx), pos = 3, 
           col = highlight_colors[as.character(idx)], cex = 0.8, font = 2)
    }
  }
}

" It is evident that those products (coffee, dark chocolate, beef herd and lamb)
present unusual values in almost all the variables."

## Try K-means on t-SNE dimensions
set.seed(42)
km_tsne <- kmeans(df_tsne, centers = 4)  
df_tsne$Cluster <- as.factor(km_tsne$cluster)

ggplot(df_tsne, aes(x = Dim1, y = Dim2, color = Cluster)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = rownames(df_tsne)), vjust = -1, size = 3) +
  ggtitle("t-SNE on K-means Clustering") +
  theme_minimal()

scaled_foodproduction$Cluster_tsne <- km_tsne$cluster  
cluster_tsne_means <- aggregate(. ~ Cluster_tsne, data = scaled_foodproduction, FUN = mean)

print(cluster_tsne_means)

"Using the two dimensions given by the t-SNE method, we do not have outliers anymore:
the indexes 32,33,34,36 now belong to full clusters."

--------------------------------------------------------------------------------
  
## K-MEDIAN
"We decided to cluster the data using the k-median algorithm to try to reduce the 
influence of the outliers 32, 33, 34 and 36 on the model."

set.seed(123)
k.median.fit <- pam(scaled_foodproduction, 4, metric = "manhattan")  
scaled_foodproduction$cluster <- factor(k.median.fit$clustering)  
cols <- colnames(scaled_foodproduction)[1:3]

plot_ly(data = scaled_foodproduction, 
        x = ~get(cols[1]), y = ~get(cols[2]), z = ~get(cols[3]), 
        color = ~cluster, colors = c("red", "blue", "green", "purple"),
        type = "scatter3d", mode = "markers") %>%
  layout(title = "3D K-Median Clustering")

"Regardless the number of clusters that we choose, the k-median is not an efficient
method to cluster our data, because the final clusters are very close to each other."

--------------------------------------------------------------------------------

## HIERARCHICAL CLUSTERING
"Compare the results of hierarchical clustering based on average, complete and 
ward linkage methods."

# Avg linkage plot
h1<-hclust(dist(scaled_foodproduction), method="average")
plot(h1, main="average linkage")
rect.hclust(h1, 4)

# Complete linkage plot 
h2<-hclust(dist(scaled_foodproduction), method="complete")
plot(h2, main="complete linkage")
rect.hclust(h2, 4)

# Ward linkage plot
h3<-hclust(dist(scaled_foodproduction), method="ward.D2")
plot(h3, main="Ward linkage")
rect.hclust(h3, 4)

# Try with 4 clusters 
average <- cutree(h1, k=4)
complete<- cutree(h2, k=4)
ward<- cutree(h3, k=4)

scaled_foodproduction$Cluster_average <- average
scaled_foodproduction$Cluster_complete <- complete
scaled_foodproduction$Cluster_ward <- ward
"Since they are quite similar, we chose one for the interpretation"


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
dev.off()

# Explore solutions
plot(scaled_foodproduction[,c(1,2,3,4)], col=average, main="average linkage")
plot(scaled_foodproduction[,c(1,2,3,4)], col=complete, main="complete linkage")
plot(scaled_foodproduction[,c(1,2,3,4)], col=ward, main="ward linkage")

table(average)
table(complete)
table(ward)

# Cluster means
means1<-aggregate(scaled_foodproduction, list(average), mean)
t(round(means1, 2))
means2<-aggregate(scale(scaled_foodproduction), list(complete), mean)
t(round(means2, 2))
means3<-aggregate(scale(scaled_foodproduction), list(ward), mean)
t(round(means3, 2))

"The first clusters includes the majority of the data, while the others have 
very few. The hierarchical method based on the Ward linkage looks more 
informative, but the number of clusters obtained (that we can see from the 
table) is only 2: increasing it to three, only one value is assigned to the 
additional cluster."

--------------------------------------

## HCPC
" Hierarchical clustering on Principal Components: we try to combine PCA and 
hierarchical clustering to see if we can get better results."

res.hcpc <- HCPC(pca_data, graph = FALSE)
res.hcpc$call$t$nb.clust # we get that 4 is the optimal number of clusters

plot(res.hcpc, choice = "tree")
plot(res.hcpc, choice = "map") # factor map: distribution of the data

# Extract centroids 
centroids <- res.hcpc$desc.var$quanti
print(centroids)

"We started by applying the hierarchical clustering method on scaled data.
After that, we used PCA to reduce data dimensionality.
Eventually, we combined PCA and clustering through the HCPC, to obtain a robust solution."
