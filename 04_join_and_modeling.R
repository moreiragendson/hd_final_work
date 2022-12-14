
library(tidyverse)
library(stargazer)


gini <- read_csv("gini.csv")
glimpse(gini)

pnad <- read_csv("raca_cor_pnad.csv")
glimpse(pnad)

docentes <- read_csv("docentes.csv")
glimpse(docentes)


completo <- pnad %>% 
  full_join(gini, by = "uf") %>% 
  full_join(docentes, by = "abbrev_state")



completo %>% 
  select(code_region, name_region) %>% 
  distinct()

completo <- completo %>%
  rename(BrPop = Branca,
         BrDoc = brancos) %>% 
  mutate(razao = BrDoc/BrPop * 100,
         sul = ifelse(code_region==4, 1,0),
         norte = ifelse(code_region==1, 1,0),
         sudeste = ifelse(code_region==3, 1,0),
         co = ifelse(code_region==5, 1,0),
         nordeste = ifelse(code_region==2, 1,0))
  

# lista de estatísticas: procurar em Help
# stargazer_stat_code_list

# sg: sem gini
fitsg1 <- lm(razao ~ nordeste, data = completo)
fitsg2 <- lm(razao ~ sul, data = completo)
fitsg3 <- lm(razao ~ norte, data = completo)
fitsg4 <- lm(razao ~ sudeste, data = completo)
fitsg5 <- lm(razao ~ co, data = completo)
# o modelo sem gini é melhor

stargazer(fitsg1,
          fitsg2,
          fitsg3,
          fitsg4,
          fitsg5,
          type = "text",
          decimal.mark = ",",
          omit.stat = "ser")



fit1 <- lm(razao ~ sul + gini, completo)
summary(fit)
fit2 <- lm(razao ~ sudeste + gini, completo)
summary(fit)
fit3 <- lm(razao ~ co + gini, completo)
summary(fit)
fit4 <- lm(razao ~ norte + gini, completo)
summary(fit)
fit5 <- lm(razao ~ nordeste + gini, completo)
summary(fit)

stargazer(fit1,
          fit2,
          fit3,
          fit4,
          fit5,
          type = "text",
          decimal.mark = ",",
          omit.stat = "ser")
