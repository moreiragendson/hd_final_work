
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

