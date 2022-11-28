

# Nesta análise, verificamos a estratificação racial por UFs
# Para isso utilizamos o percentual de docentes brancos


# join sf -----------------------------------------------------------------

sf_uf <- read_state(year = 2010)

names(sf_uf)

ies %>% 
  pull(ano) %>% 
  unique()

# gráfico (percentual de brancos) -----------------------------------------


maps_fct <- function(q, year){
  
  ies %>% 
    filter(ano=={{year}}) %>%
    group_by(cod_uf) %>% 
    summarise(perc = sum(doc_brancos)/sum(doc_ex_total)) %>% 
    mutate(perc_cat = quantcut(perc, q={{q}})) %>% 
    left_join(sf_uf, by = c("cod_uf" =  "code_state")) %>% 
    ggplot()+
    geom_sf(aes(fill=perc_cat, geometry=geom), color="black", size=0.2, alpha=1)+
    theme_minimal()+
    scale_fill_brewer(palette = "Blues", direction = 1)+
    style_sf()+
    labs(title = {{q}})
  
}  

mapas <- map(.x=2:9, .f=maps_fct, year=2020)

mapas_comparados <- (mapas[[1]] | mapas[[2]] | mapas[[3]]) / 
  (mapas[[4]] | mapas[[5]] | mapas[[6]]) /
  (mapas[[7]] | mapas[[8]])


# gráfico escolhido -------------------------------------------------------

# 9 categorias
mapas[[8]]+style_sf()+
  labs(title = "Percentual de docentes brancos em exercício por UF",
       subtitle = "Censo da Educação Superior 2020 (Inep)",
       caption = "github.com/moreiragendson")

