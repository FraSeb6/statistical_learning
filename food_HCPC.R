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
<<<<<<< Updated upstream:food_HCPC.R
scaled_data[, -1] <- scale(Food_Production[, -1])
=======
scaled_data[,-1] <- scale(Food_Production[, -1])


pca_res <- prcomp(scaled_data[, 2:8], scale. = TRUE)
>>>>>>> Stashed changes:food_code.R

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
