

# Estatísticas sobre os docentes por raça cor -----------------------------

ies <- read_regex("(IES)")

ies %<>% 
  select(NU_ANO_CENSO, starts_with("QT_DOC"), matches("(IES)"))

glimpse(ies)

ies <- ies %>% 
  rename(ano = NU_ANO_CENSO,
         doc_brancos = QT_DOC_EX_BRANCA,
         doc_pretos = QT_DOC_EX_PRETA,
         doc_pardos = QT_DOC_EX_PARDA,
         doc_indigenas = QT_DOC_EX_INDIGENA,
         doc_amarelos = QT_DOC_EX_AMARELA,
         doc_na = QT_DOC_EX_COR_ND)

doc_racacor <- ies %>% 
  group_by(ano) %>% 
  summarise(brancos = sum(doc_brancos, na.rm = T),
            pretos = sum(doc_pretos, na.rm = T),
            pardos = sum(doc_pardos, na.rm = T),
            indigenas = sum(doc_indigenas, na.rm = T),
            amarelos = sum(doc_amarelos, na.rm = TRUE),
            na = sum(doc_na, na.rm = TRUE)) %>% 
  mutate(negros = pretos + pardos,
         outros = indigenas + amarelos)

doc_racacor %>% 
  filter(ano != 2009) %>% 
  ggplot(aes(ano))+
  geom_line(aes(y=brancos), color= "grey")+
  geom_line(aes(y=pretos), color = "black")+
  geom_line(aes(y=pardos), color = "brown")+
  geom_line(aes(y=indigenas), color = "green")+
  geom_line(aes(y=amarelos), color = "yellow")+
  geom_line(aes(y=na), linetype = "dashed")

doc_racacor %>% 
  filter(ano != 2009) %>% 
  ggplot(aes(ano))+
  geom_line(aes(y=brancos), color= "grey")+
  geom_line(aes(y=negros), color = "brown")+
  geom_line(aes(y=outros), color = "green")+
  geom_line(aes(y=na), linetype = "dashed")


