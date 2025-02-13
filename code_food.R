library(factoextra)
library(dplyr)

Food_Production <- read.csv("Food_Production.csv")
Food_Production <- as.data.frame(Food_Production)

Food_Production <- Food_Production %>%
  rename(food_product = 'Food.product',
         land_use_change = 'Land.use.change',
         animal_feed = 'Animal.Feed',
  )


pca_res <- prcomp(Food_Production[, 1:4], scale. = TRUE)
