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

pca_res <- prcomp(Food_Production[, 2:8], scale. = TRUE)


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
res.hcpc <- HCPC(res.pca, graph = FALSE)

# Number of clusters chosen
res.hcpc$call$t$nb.clust

# Numberof clusters chosen
plot(res.hcpc, choice = "tree")

# Visualize clusters in factor map
plot(res.hcpc, choice = "map")
