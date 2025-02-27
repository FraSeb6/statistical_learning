library(dplyr)
library(factoextra)
library(FactoMineR)
library(factoextra)
library(rattle)
library(cluster)
library(caret)
library(plotly)


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

# Histograms, Boxplot: check variables' distributions
par(mfrow=c(5,5), mar=c(2, 2, 2, 2))
for(i in 2:23) {
  hist(Food_Production[, i], main=names(Food_Production)[i], col="lightblue")
}
dev.off()

boxplot(Food_Production[, -1], main="Distribuzione delle Variabili", las=2, col="lightblue")

# Density plot: help visualization
plot(density(Food_Production[,2], na.rm=TRUE), col=rainbow(23)[1], lwd=2, main="Densità delle Variabili")
for (i in 3:ncol(Food_Production)) {
  lines(density(Food_Production[,i], na.rm=TRUE), col=rainbow(23)[i], lwd=2)
}
legend("topright", legend=names(Food_Production)[2:23], col=rainbow(23), lwd=2, cex=0.6)

# Focus on the last 13 variables
plot(density(Food_Production[,10], na.rm=TRUE), col=rainbow(23)[1], lwd=2, main="Densità delle Variabili")
for (i in 3:ncol(Food_Production)) {
  lines(density(Food_Production[,i], na.rm=TRUE), col=rainbow(23)[i], lwd=2)
}
legend("topright", legend=names(Food_Production)[10:23], col=rainbow(23), lwd=2, cex=0.6)

"The distribution of some variables is very skewed to the right: converting them
to logarithms could change this asymmetry. We cannot keep the dataset like this if 
we want to compare variables.
We decide to convert variables 13:15 and 21:23 (those that reach 1,000 as maximum
value) of our dataset into their logarithmic form to adjust the skeweness."

Foodproduction_log <- Food_Production
Foodproduction_log[, 21:23] <- log1p(Food_Production[, 21:23])
Foodproduction_log[, 13:15] <- log1p(Food_Production[, 13:15])

plot(density(Foodproduction_log[,2], na.rm=TRUE), col=rainbow(23)[1], lwd=2, main="Densità delle Variabili")
for (i in 3:ncol(Foodproduction_log)) {
  lines(density(Foodproduction_log[,i], na.rm=TRUE), col=rainbow(23)[i], lwd=2)
}
legend("topright", legend=names(Food_Production)[2:23], col=rainbow(23), lwd=2, cex=0.6)

"We see from the density plot that we have many different peaks: this could be
a good sign for the clustering process that we are going to explore, since it is
likely that our data is naturally split in different groups."

# Check skewness: repeat visualization
par(mfrow=c(5,5), mar=c(2, 2, 2, 2))
for(i in 2:23) {
  hist(Foodproduction_log[, i], main=names(Foodproduction_log)[i], col="lightblue")
}
dev.off()

boxplot(Foodproduction_log[, -1], main="Distribuzione delle Variabili", las=2, col="lightblue")

"Some variables still have very wide ranges compared to others (to the first 8 for
example, so we scale them."

# Scale data
scaled_foodproduction <- Foodproduction_log
scaled_foodproduction[, -1] <- scale(Foodproduction_log[, -1])
scaled_foodproduction <- scaled_foodproduction[, 2:23]

# Check the scaling 
plot(density(scaled_foodproduction[,2], na.rm=TRUE), col=rainbow(23)[1], lwd=2, main="Densità delle Variabili")
for (i in 3:ncol(scaled_foodproduction)) {
  lines(density(scaled_foodproduction[,i], na.rm=TRUE), col=rainbow(23)[i], lwd=2)
}
legend("topright", legend=names(Food_Production)[2:23], col=rainbow(23), lwd=2, cex=0.6)


# Verify correlation between variables
corr_matrix <- cor(scaled_foodproduction)
heatmap(corr_matrix, main="Correlation map")

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

## K-MEANS
# Find optimal number of clusters
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(123)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(scaled_foodproduction, nc=10)

"It looks from the plot that 4 would be the optimal number of cluster"

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

" We can also try to look at the 3D plot: use the first three principal components"
pca_result <- prcomp(scaled_foodproduction, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x[, 1:3])  
set.seed(123)  # Per riproducibilità
kmeans_result <- kmeans(pca_data, centers = 5, nstart = 25)  # Cambia 'centers' se necessario

# Aggiungi i cluster ai dati PCA
pca_data$Cluster <- as.factor(kmeans_result$cluster)
pca_data$RowIndex <- rownames(scaled_foodproduction)  

# Plotta con hover text che mostra l'indice della riga
plot_ly(pca_data, 
        x = ~PC1, 
        y = ~PC2, 
        z = ~PC3, 
        color = ~Cluster, 
        text = ~paste("Row:", RowIndex),  # Mostra l'indice della riga
        colors = c("red", "blue", "green"),
        type = "scatter3d", 
        mode = "markers") %>%
  layout(title = "Cluster in PCA Space")


# Verify that rows 32-33-34-36 are actually an outlier
boxplot_info <- boxplot(scaled_foodproduction, main = "Boxplot: highlight outliers", col = "lightblue")  
# Definizione degli indici e dei colori corrispondenti
highlight_indices <- c(32, 33, 34, 36)
highlight_colors <- c("red", "green", "blue", "purple")  # Colori per ogni indice
names(highlight_colors) <- highlight_indices  # Associa i colori agli indici

for (i in 1:ncol(scaled_foodproduction)) {
  variable_values <- scaled_foodproduction[, i] 
  outliers <- boxplot_info$out[boxplot_info$group == i]  
  outlier_indices <- which(variable_values %in% outliers)  
  
  # Controlla se gli indici specificati sono tra gli outlier
  for (idx in highlight_indices) {
    if (idx %in% outlier_indices) {
      text(i, variable_values[idx], labels = as.character(idx), pos = 3, 
           col = highlight_colors[as.character(idx)], cex = 0.8, font = 2)
    }
  }
}

" It is evident that those products (coffee, dark chocolate, beef herd and lamb)
represent in some ways outliers."

--------------------------------------------------------------------------------
  
## K-MEDIAN
"We decided to cluster the data using the k-median algorithm to try to reduce the 
influence of the outliers 32, 33, 34 and 36 on the model."

set.seed(123)
k.median.fit <- pam(scaled_foodproduction, 4, metric = "manhattan")  
scaled_foodproduction$cluster <- factor(k.median.fit$clustering)  # Converti cluster in fattore

# Seleziona le prime tre colonne per il grafico 3D
cols <- colnames(scaled_foodproduction)[1:3]

# Grafico interattivo
plot_ly(data = scaled_foodproduction, 
        x = ~get(cols[1]), y = ~get(cols[2]), z = ~get(cols[3]), 
        color = ~cluster, colors = c("red", "blue", "green", "purple"),
        type = "scatter3d", mode = "markers") %>%
  layout(title = "3D K-Median Clustering")

"Regardless the number of clusters that we choose, the k-median is not an efficient
method to cluster our data, because the final clusters are very close to each other."
