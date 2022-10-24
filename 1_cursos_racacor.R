

# source R scripts --------------------------------------------------------

source("0_libraries.R")
source("0_functions.R")

cursos <- read_regex("(CURSOS)")

names(cursos)

cursos %>% 
  pull(NO_CINE_ROTULO) %>%
  unique()

cursos <- cursos %>% 
  rename(ano = NU_ANO_CENSO,
         ingressantes = QT_ING, 
         cod_especifica = CO_CINE_AREA_ESPECIFICA,
         especifica = NO_CINE_AREA_ESPECIFICA,
         cod_geral = CO_CINE_AREA_GERAL,
         geral = NO_CINE_AREA_GERAL)


cursos %>% 
  select(matches("(CINE)"), ano)  %>% 
  names()


cursos %>% 
  group_by(NO_CINE_ROTULO) %>% 
  summarise(brancos = sum(QT_ING_BRANCA)) %>% 
  slice_max(order_by = brancos, n = 20) %>% 
  ggplot()+
  geom_col(aes(fct_reorder(NO_CINE_ROTULO, brancos), brancos))+
  coord_flip()

centis <- function(data)data %>% mutate(perc_ingressantes = qtd_ingressantes/sum(qtd_ingressantes, na.rm = T))

get_per_area <- function(data, area){
  
  data %>% 
    group_by(ano,  {{area}}  ) %>% 
    summarise(qtd_ingressantes = sum(ingressantes, na.rm = T)) %>% 
    split(.$ano) %>%
    map_dfr(.f = centis)
  
}
ing_geral <- get_per_area(cursos, geral)



ing_geral%>% 
  ggplot(aes(x=ano, y=qtd_ingressantes, color=geral))+
  geom_line()+
  style_line()+
  scale_x_continuous(labels = lab_years(ing_geral, ano), breaks = seq(2009, 2020, 1))



# plot specific areas of knowledge ----------------------------------------

plot_specific_areas <- function(vetor){
  
  ing_especifica <- cursos %>% 
    filter(geral=={{vetor}}) %>% 
    get_per_area(especifica)
  
  
  
  ing_especifica%>% 
    ggplot(aes(x=ano, y=qtd_ingressantes, color=especifica))+
    geom_line()+
    geom_point()+
    style_line()+
    scale_y_continuous(labels = scales::comma_format(big.mark = ".",
                                                     decimal.mark = ","))+
    scale_x_continuous(labels = lab_years(ing_especifica, ano), breaks = seq(2009, 2020, 1))+
    labs(title = "Ingressantes em IES por área específica",
         subtitle = str_c("Área Geral: ", {{vetor}}),
         x="Ano",
         y="Ingressantes (n)",
         color="Área específica:",
         caption = "Elaboração: github.com/moreiragendson; Fonte: Censo da Educação Superior (INEP)")+
    guides(color=guide_legend(ncol = 2, byrow=T))
  
}


cursos %>%
  pull(geral) %>% 
  unique() %>% 
  map(.f=plot_specific_areas)

cursos %>% 
  filter(especifica %in% c("Ciências sociais e comportamentais",
                           "Programas interdisciplinares abrangendo ciências sociais, comunicação e informação")) %>% 
  pull(NO_CINE_ROTULO) %>% 
  unique()

cursos %>% 
  filter(especifica == "Comunicação e informação") %>% 
  pull(NO_CINE_ROTULO) %>% 
  unique()
  
cursos %>% 
  filter(especifica == "Negócios e administração") %>% 
  pull(NO_CINE_ROTULO) %>% 
  unique()


cursos %>% 
  filter(especifica=="Ciências sociais e comportamentais") %>% 
  group_by(ano, especifica, NO_CINE_ROTULO) %>% 
  summarise(ingressantes = sum(ingressantes, na.rm = T)) %>% 
  ggplot(aes(ano, ingressantes, color=NO_CINE_ROTULO))+
  geom_line()+
  scale_x_continuous(labels = lab_years(cursos, ano), breaks = seq(2009,2020,1))+
  style_line()+
  labs(x="Ano",
       y="Número de ingressantes",
       color = "Curso")

















