

# libraries ---------------------------------------------------------------

library(tidyverse)
library(janitor)
library(magrittr)
library(geobr)
library(gtools)
library(patchwork)

# functions ---------------------------------------------------------------

read_regex <- function(reg, .f = read_csv2){
  
  files <- dir("data/", pattern =   {{reg}}  )
  
  files <- str_c("data/", files)
  
  map_dfr(.x=files, .f=  {{.f}}   , locale = locale(encoding = "ISO-8859-1"))
  
}


# style functions ---------------------------------------------------------

lab_years <- function(data, year){
  
  var <- data %>% 
    pull({{year}})
  
  seq(  min(var),
        max(var), 1)
  
} 


style_line <- function(...) {
  
  theme_minimal()+
    theme(plot.title = element_text(family = "serif", face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(family = "serif", size = 8, hjust = 0.5),
          plot.caption = element_text(size = 10, family = "serif", face = "bold"),
          legend.text = element_text(family = "serif", size = 10),
          legend.title = element_text(family = "serif", face = "bold", size = 10),
          axis.title = element_text(family = "serif", size = 10, face = "italic"),
          axis.text.x = element_text(face = "bold", angle = 45, size = 8),
          axis.text.y = element_text(size = 8),
          legend.position = "bottom")
  
}


style_sf <- function(legenda = FALSE){
  
  legend_position <- ifelse(  legenda  == TRUE, "bottom" , "none")
  
  theme(plot.title = element_text(family = "serif", face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(family = "serif", size = 8, hjust = 0.5),
        plot.caption = element_text(face = "bold", size = 10),
        plot.caption.position = "plot", # caption se alinha ao plot
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = legend_position,
        legend.title = element_text(face = "bold"))
  
}


# read ies ----------------------------------------------------------------

ies <- read_regex("(IES)")

# rename vars -------------------------------------------------------------

ies <- ies %>% 
  rename(ano = NU_ANO_CENSO,
         doc_brancos = QT_DOC_EX_BRANCA,
         doc_pretos = QT_DOC_EX_PRETA,
         doc_pardos = QT_DOC_EX_PARDA,
         doc_indigenas = QT_DOC_EX_INDIGENA,
         doc_amarelos = QT_DOC_EX_AMARELA,
         doc_na = QT_DOC_EX_COR_ND,
         doc_ex_total = QT_DOC_EXE,
         cod_uf = CO_UF_IES,
         cod_municipio = CO_MUNICIPIO_IES,
         ies_sigla = SG_IES,
         ies_nome = NO_IES,
         ies_cod = CO_IES)


# new vars ----------------------------------------------------------------

ies <- ies %>% 
  mutate(doc_negros = doc_pretos + doc_pardos,
         doc_outros = doc_amarelos + doc_indigenas,
         doc_stricto_sensu = QT_DOC_EX_MEST + QT_DOC_EX_DOUT)



doc_raca_cor <- ies %>% 
  filter(ano != 2009) %>% 
  group_by(ano) %>% 
  summarise(negros = sum(doc_negros, na.rm = T),
            brancos = sum(doc_brancos, na.rm = T),
            outros = sum(doc_outros, na.rm = T),
            na = sum(doc_na, na.rm = T)) %>% 
  pivot_longer(all_of(c("negros", "brancos", "outros", "na")),
               names_to = "racacor",
               values_to = "count") %>% 
  group_by(ano, racacor) %>% 
  summarise(count = sum(count)) %>% 
  mutate(perc = count/sum(count))


docentes_per_cor <- ies %>% 
  filter(ano == 2020) %>% 
  group_by(cod_uf) %>% 
  summarise(negros = sum(doc_negros, na.rm = T),
            brancos = sum(doc_brancos, na.rm = T),
            outros = sum(doc_outros, na.rm = T),
            na = sum(doc_na, na.rm = T))



docentes_per_cor <- docentes_per_cor %>% 
  left_join(geobr::read_state(year=2010),
            by = c("cod_uf" = "code_state")) %>% 
  as_tibble() %>%
  mutate(total = brancos + negros + outros + na,
         PercDocBr = brancos/total) %>% 
  select(cod_uf, abbrev_state, brancos, PercDocBr)

write_csv(docentes_per_cor, "docentes.csv")

# checking
doc_raca_cor %>% 
  filter(ano == 2010) %>% 
  pull(perc) %>% 
  sum()


doc_raca_cor <- doc_raca_cor %>% 
  mutate(racacor = case_when(racacor=="brancos" ~ "Brancos",
                             racacor=="negros" ~ "Negros",
                             racacor=="outros" ~ "Outros",
                             racacor=="na"~ as.character(NA)),
         racacor = fct_explicit_na(racacor, "Sem informação/Não declarada"))

# série histórica (percentual) --------------------------------------------


doc_raca_cor_perc <- doc_raca_cor %>% 
  ggplot(aes(ano, perc, color=racacor))+
  geom_line(linetype="solid")+
  geom_point()+
  scale_x_continuous(breaks = lab_years(doc_raca_cor, ano))+
  scale_y_continuous(labels = scales::percent, breaks = seq(0,0.6,0.1))+
  style_line()+
  scale_color_brewer(type = "qual",
                     palette = "Dark2",
                     guide = guide_legend(ncol = 2))+
  labs(title = "Docentes em IES por raça/cor",
       x = "Ano",
       y = "Docentes em exercício",
       color="Cor ou raça",
       caption = "Censo da Educação Superior (Inep)")

doc_raca_cor_perc

# série histórica (n) -----------------------------------------------------


doc_raca_cor_count <- doc_raca_cor %>%
  ggplot(aes(ano, count, color=racacor))+
  geom_line(linetype="solid")+
  geom_point()+
  scale_x_continuous(breaks = lab_years(doc_raca_cor, ano))+
  # transformar em função
  scale_y_continuous(labels = scales::comma_format(big.mark = ".",
                                                   decimal.mark = ","))+
  style_line() +
  scale_color_brewer(type = "qual",
                     palette = "Dark2",
                     guide = guide_legend(ncol = 2))+
  labs(title = "Docentes em IES por raça/cor",
       x="Ano",
       y="Docentes em exercício",
       caption="Censo da Educação Superior (Inep)",
       color="Cor ou raça")

doc_raca_cor_count

doc_raca_cor_count | doc_raca_cor_perc



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
       caption = "Censo da Educação Superior 2020 (Inep)")



gini <- read_csv("gini.csv")
glimpse(gini)

pnad <- read_csv("raca_cor_pnad.csv")
glimpse(pnad)

docentes <- read_csv("docentes.csv")
glimpse(docentes)


completo <- pnad %>% 
  full_join(gini, by = "uf") %>% 
  full_join(docentes, by = "abbrev_state") %>% 
  full_join(sf_uf, by = "abbrev_state")

completo %>% 
  mutate(gini2 = gtools::quantcut(gini,9)) %>% 
  ggplot()+
  geom_sf(aes(geometry=geom, fill=gini2))+
  scale_fill_brewer(direction = 1, palette = "Blues")+
  theme_minimal()+
  style_sf(F)+
  labs(caption = "PNAD 2010 (SIDRA IBGE)",
       title = "Índice de Gini")


