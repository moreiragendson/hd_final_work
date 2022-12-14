
library(tidyverse)
library(PNADcIBGE)
library(srvyr)
library(stringi)


vars <- c("V2010",
          "UF",
          "Estrato",
          "V1028",
          "Ano",
          "Trimestre",
          "UPA")

pnadc <- PNADcIBGE::get_pnadc(2022, quarter = 3, design = F,
                              vars = vars
                              )

names(pnadc)

pnadc %>% 
  pull(UF) %>% 
  unique()

pnadc %>% 
  pull(UF) %>% 
  levels()

pnadc <- pnadc %>% 
  rename(cor = V2010)

survey <- pnadc %>% 
  as_survey_design(
  ids = UPA,
  strata = Estrato,
  weights = V1028,
  nest = TRUE
)


cor_pnad <- survey %>% 
  group_by(UF, cor) %>% 
  summarise(n = survey_total()) %>% 
  select(-n_se) %>% 
  tidyr::pivot_wider(
    names_from = cor,
    values_from = n
  ) %>% dplyr::mutate(Ignorado = ifelse(is.na(Ignorado), 0, Ignorado))

cor_pnad <- cor_pnad %>% 
  mutate(uf = stri_trans_general(UF, "Latin-ASCII"),
         uf = str_to_lower(uf))

sf_uf <- read_state(year = 2010)

sf_uf <- sf_uf %>% 
  mutate(uf = stri_trans_general(name_state, "Latin-ASCII"),
         uf = str_to_lower(uf))


glimpse(sf_uf)

cor_pnad <- cor_pnad %>% 
  left_join(sf_uf, by = "uf")

glimpse(cor_pnad)

cor_pnad <- cor_pnad %>% 
  dplyr::mutate(Total = Branca + Preta + Amarela + Parda + Indígena + Ignorado,
                PercBr = Branca/Total)


maps_fct2 <- function(q){
  
  cor_pnad %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(perc_cat = gtools::quantcut(PercBr, q={{q}})) %>%
    ggplot()+
    geom_sf(aes(fill=perc_cat, geometry=geom), color="black", size=0.2, alpha=1)+
    theme_minimal()+
    scale_fill_brewer(palette = "Blues", direction = 1)+
    style_sf()+
    labs(title = {{q}})
  
}  


mapas <- map(.x=2:9, .f=maps_fct2)

mapas_comparados <- (mapas[[1]] | mapas[[2]] | mapas[[3]]) / 
  (mapas[[4]] | mapas[[5]] | mapas[[6]]) /
  (mapas[[7]] | mapas[[8]])

mapas[[8]]+style_sf()+
  labs(title = "Percentual de brancos por UF",
       caption = "PNAD Contínua 2022/3º trimestre (IBGE)")

glimpse(cor_pnad)

cor_pnad %>% 
  as_tibble() %>% 
  select(uf, abbrev_state, name_region, code_region, PercBr, Branca) %>% 
  write_csv("raca_cor_pnad.csv")
