

# source R scripts --------------------------------------------------------

source("0_libraries.R")
source("0_functions.R")

cursos <- read_regex("(CURSOS)")

names(cursos)

cursos %>% 
  pull(NO_CINE_ROTULO) %>%
  unique()


cursos %>% 
  filter(NU_ANO_CENSO==2020) %>% 
  group_by(NO_CINE_ROTULO) %>% 
  summarise(brancos = sum(QT_ING_BRANCA)) %>% 
  slice_max(order_by = brancos, n = 20) %>% 
  ggplot()+
  geom_col(aes(fct_reorder(NO_CINE_ROTULO, brancos), brancos))+
  coord_flip()
