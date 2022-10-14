
# read ies ----------------------------------------------------------------

ies <- read_regex("(IES)")

names(ies)

ies %>% 
  select(starts_with("QT_DOC"), matches("(IES)")) %>% 
  names()

