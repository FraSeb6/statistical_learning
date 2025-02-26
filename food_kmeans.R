library(factoextra)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(rattle)
library(cluster)

Food_Production <- read.csv("Food_Production.csv")

Food_Production <- Food_Production %>%
  rename(food_product = 'Food.product',
         land_use_change = 'Land.use.change',
         animal_feed = 'Animal.Feed',
  )

Food_Production[-1] <- lapply(Food_Production[-1], function(x) {
  if (is.numeric(x)) {# Se la colonna è numerica, sostituire i valori NA con la media
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  }
  return(x)
})


## DESCRIPTIVE STATISTICS

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


"The distribution of some variables is very skewed to the right: we convert 
the last three variables of our dataset into their logarithmic form to adjust the skeweness.
After converting them and visualizing the boxplot, we decided to convert also 13:15."

Foodproduction_log <- Food_Production
Foodproduction_log[, 21:23] <- log1p(Food_Production[, 21:23])
Foodproduction_log[, 13:15] <- log1p(Food_Production[, 13:15])

# Check skewness: repeat visualization
par(mfrow=c(5,5), mar=c(2, 2, 2, 2))
for(i in 2:23) {
  hist(Foodproduction_log[, i], main=names(Foodproduction_log)[i], col="lightblue")
}
dev.off()

boxplot(Foodproduction_log[, -1], main="Distribuzione delle Variabili", las=2, col="lightblue")


# Scale data
scaled_foodproduction <- Foodproduction_log
scaled_foodproduction[, -1] <- scale(Foodproduction_log[, -1])
scaled_foodproduction <- scaled_foodproduction[, 2:8]

# Find optimal number of clusters
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(123)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(scaled_foodproduction, nc=10)

"It looks from the plot that 6 is the optimal number of cluster"

set.seed(123)
k.means.fit <- kmeans(scaled_foodproduction, 6)
str(k.means.fit)
scaled_foodproduction$cluster <- k.means.fit$cluster
table(k.means.fit$cluster, scaled_foodproduction$cluster)


clusplot(scaled_foodproduction, k.means.fit$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

"From the table of the k-means, we can see that the fourth cluster has only one observation."
"We can also see from the plot that that cluster contains the number 34, which is probably an outlier.
We can try to apply the k-median method, or re-try the k-means after dropping the outlier."


# Verify that row 34 is actually an outlier
boxplot_info <- boxplot(scaled_foodproduction, main = "Boxplot: highlight outliers", col = "lightblue")  

for (i in 1:ncol(scaled_foodproduction)) {
  variable_values <- scaled_foodproduction[, i] 
  outliers <- boxplot_info$out[boxplot_info$group == i]  
  outlier_indices <- which(variable_values %in% outliers)  
  
  # Controlla se la riga 34 è tra gli outlier
  if (34 %in% outlier_indices) {
    text(i, variable_values[34], labels = "34", pos = 3, col = "blue", cex = 0.8, font = 2)
  }
}



" From the boxplot I can see that 34 is always present as outlier in every variable, 
except for the last one which does not present outliers."

# Drop the outlier
scaled_foodproduction <- scaled_foodproduction[-34, ] 

wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(123)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(scaled_foodproduction, nc=10)

# Try again the k-means algorithm 
set.seed(123)
k.means.fit.woutlier <- kmeans(scaled_foodproduction, 4)
str(k.means.fit.woutlier)
scaled_foodproduction$cluster <- k.means.fit.woutlier$cluster
table(k.means.fit.woutlier$cluster, scaled_foodproduction$cluster)

clusplot(scaled_foodproduction, k.means.fit.woutlier$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

"The clusters look different from before, but now it appears that they fit the data better."

