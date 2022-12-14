
library(readxl)
library(tidyverse)

gini <- read_excel("Indice de Gini.xlsx") %>% 
  janitor::clean_names()

gini <- gini %>% 
  mutate(uf = stringi::stri_trans_general(unidades_da_federacao, "Latin-ASCII"),
         uf = str_to_lower(uf))

gini <- gini %>% 
  select(uf, gini=x2010)

gini
write_csv(gini, "gini.csv")
