
# join sf -----------------------------------------------------------------

ies <- ies %>% 
  rename(cod_uf = CO_UF_IES,
         cod_municipio = CO_MUNICIPIO_IES)

sf_mun <- read_municipality(year = 2020)

names(sf_mun)

glimpse(sf_mun)
names(ies)

ies <- right_join(ies, sf_mun, by = c("cod_municipio" = "code_muni"))


ies %>% 
  filter(abbrev_state=="MG") %>% 
  ggplot()+
  geom_sf(aes(fill=doc_brancos, geometry=geom), color="black")+
  theme_light() + # aqui!
  scale_fill_distiller(palette = "Reds", # e aqui!
                       direction = 1)
