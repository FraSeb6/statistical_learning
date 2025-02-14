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

scaled_data <- Food_Production
scaled_data[, -1] <- scale(Food_Production[, -1])
scaled_data <- scaled_data[, 2:8]

set.seed(123)
k.means.fit <- kmeans(scaled_data, 4)
str(k.means.fit)
scaled_data$cluster <- k.means.fit$cluster
table(k.means.fit$cluster, scaled_data$cluster)



wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(123)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(scaled_data, nc=10)

clusplot(scaled_data, k.means.fit$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

# go through all the plots
for(k in 2:6){
  k.m.fit <- kmeans(scaled_data, k)
  clusplot(scaled_data, k.m.fit$cluster, 
           main=sprintf("2D representation of the Cluster solution\n k = %d",k),
           color=TRUE, shade=TRUE,
           labels=2, lines=0)}
