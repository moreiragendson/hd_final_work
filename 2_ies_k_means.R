

# source R scripts anteriores ---------------------------------------------

source("0_libraries.R")
source("0_functions.R")
source("1_ies_doc_racacor.R")
source("1_ies_doc_racacor_geo.R")

names(ies)

vinte <- ies %>% 
  filter(ano==2020)


# artigo tidymodels -------------------------------------------------------

centers <- tibble(
  cluster = factor(1:3), 
  num_points = c(100, 150, 50),  # number points in each cluster
  x1 = c(5, 0, -3),              # x1 coordinate of cluster center
  x2 = c(-1, 1, -2)              # x2 coordinate of cluster center
)

labelled_points <- 
  centers %>%
  mutate(
    x1 = map2(num_points, x1, rnorm),
    x2 = map2(num_points, x2, rnorm)
  ) %>% 
  select(-num_points) %>% 
  unnest(cols = c(x1, x2))

points <- 
  labelled_points %>% 
  select(-cluster)

kclust <- kmeans(points, # matriz numérica
                 centers = 3)
kclust
