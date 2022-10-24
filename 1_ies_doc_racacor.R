
# Estatísticas sobre os docentes por raça cor ao longo dos anos


# source functions --------------------------------------------------------

source("0_functions.R")
source("0_style_functions.R")

# data wrangling ----------------------------------------------------------


ies <- read_regex("(IES)")

ies %<>% 
  select(NU_ANO_CENSO, starts_with("QT_DOC"), matches("(IES)"))

glimpse(ies)



# check if new colums have missing values
ies %>% 
  filter(is.na(doc_negros) & ano!=2009) %>% 
  select(starts_with("doc"))

names(ies)

glimpse(ies)


# summarise data ----------------------------------------------------------


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
  labs(title = "Docentes em IES por raça/cor",
       subtitle = "Censo da Educação Superior (Inep)",
       x = "Ano",
       y = "Docentes em exercício",
       color="Cor ou raça",
       caption = "github.com/moreiragendson")

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
  labs(title = "Docentes em IES por raça/cor",
       subtitle = "Censo da Educação Superior (Inep)",
       x="Ano",
       y="Docentes em exercício",
       caption="github.com/moreiragendson",
       color="Cor ou raça")

doc_raca_cor_count
