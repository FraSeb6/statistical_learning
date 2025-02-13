library(factoextra)
library(dplyr)

Food_Production <- read.csv("Food_Production.csv")
Food_Production <- as.data.frame(Food_Production)

Food_Production <- Food_Production %>%
  rename(food_product = 'Food.product',
         land_use_change = 'Land.use.change',
         animal_feed = 'Animal.Feed',
  )


pca_res <- prcomp(Food_Production[, 2:8], scale. = TRUE)
fviz_pca_ind(
  pca_res,
  geom.ind = "point",
  col.ind = Food_Production$food_product, # color by species
  addEllipses = TRUE,# color by species
  # confidence ellipses for each group
  legend.title = "Product"
)

# Visualize variables only
fviz_pca_var(
  pca_res,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)
