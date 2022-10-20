

# source functions and libraries ------------------------------------------

source("0_libraries.R")
source("0_functions.R")
source("1_ies_read.R")

ies <- read_regex("(IES)")

names(ies)

ies %>% 
  pull(NO_IES) %>% 
  length() == ies %>% 
  pull(NO_IES) %>% 
  unique() %>% 
  length()


ies %>% 
  select(starts_with("QT")) %>% 
  names()

glimpse(ies)


# IES com mais docentes com stricto sensu ---------------------------------

doc_st_se <- ies %>% 
  mutate(ies_sigla = fct_other(ies_sigla, drop = NA)) %>% 
  filter(ano==2020) %>%
  group_by(ies_sigla) %>% 
  summarise(n = sum(doc_stricto_sensu),
            perc = sum(doc_stricto_sensu)/sum(doc_ex_total))

# count
doc_st_se %>% 
  slice_max(n, n = 30) %>%
  ggplot()+
  geom_col(aes(
    fct_reorder(ies_sigla, n), n))+
  coord_flip()

# percentual ao longo dos anos --------------------------------------------

doc_st_se2 <- ies %>% 
  filter(ano!=2009) %>% 
  group_by(ano) %>% 
  summarise(n = sum(doc_stricto_sensu, na.rm = TRUE),
            perc = sum(doc_stricto_sensu, na.rm = TRUE)/sum(doc_ex_total, na.rm = TRUE))

# Aqui estamos considerando quaisquer cursos de IES, inclusive cursos de tecnólogo
doc_st_se2 %>% 
  ggplot(aes(ano, perc))+
  geom_line()+
  scale_x_continuous(breaks = lab_years(doc_st_se2, ano))+
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 1, 0.1),
                     limits = c(0.5, 1))+
  geom_point()+
  style_line()+
  geom_text(aes(label = scales::percent(perc,
                                        big.mark = ".",
                                        decimal.mark = ",",
                                        suffix = "%",
                                        accuracy = 1)),
            vjust = -2)+
  
  labs(y="Docentes em exercício",
       x="Ano",
       subtitle = "Censo da Educação Superior (Inep)",
       title = "Docentes com pós-graduação stricto-sensu",
       caption = "github.com/moreiragendson")

  

# geo docentes com stricto sensu ------------------------------------------

geo_st_se <- ies %>% 
  filter(ano!=2009) %>% 
  group_by(cod_uf, ano) %>% 
  summarise(n = sum(doc_stricto_sensu, na.rm = TRUE),
            perc = sum(doc_stricto_sensu, na.rm = TRUE)/sum(doc_ex_total, na.rm = TRUE))

listas_geo <- geo_st_se %>% 
  left_join(sf_uf, by = c("cod_uf" = "code_state")) %>% 
  mutate(ano = as.factor(ano)) %>% 
  split(f=.$ano)


create_cat <- function(data, q = 4){
  
  data %>% 
    ungroup() %>% 
    mutate(perc_cat = quantcut(perc, q = {{q}}))
  
}


listas_geo <- listas_geo %>% 
  map(.f=create_cat, q = 7) 

fct_sf <- function(data){
  
  ano <- data %>% 
    pull(ano) %>% 
    unique()
  
  data %>% 
    ggplot(aes(fill=perc_cat))+
    geom_sf(aes(geometry=geom))+
    scale_fill_brewer(palette = "Blues")+
    style_sf(T)+
    labs(title = ano)
  
}





listas_geo %>% 
  map(.f= fct_sf)


