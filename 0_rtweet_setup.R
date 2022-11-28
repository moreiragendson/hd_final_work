vignette("auth", package = "rtweet")

library(rtweet)


API_KEY <- "6N3saKWCeW99BuTqHzlDHoxtg"
API_KEY_SECRET <- "GCyjVJK9kcD0u6eNuqqhn9QX5nk56bduwjWcBHPTX5RFHufFAV"
BEARER_TOKEN <- "AAAAAAAAAAAAAAAAAAAAAOVSjgEAAAAALa04RmmGzXcM2RBWwdDFz2m9SGA%3DJzC8pKg32PCtGS7tEnA74cvNgS5bwlwr7CbKQI5YahTjn9zml1"
ACCESS_TOKEN <- "1245022867538018305-Ev7lQq41K2axYrfCGUUxRxkH2SXHqc"
ACCESS_TOKEN_SECRET <- "jvgbQ09887TJut2n74Hfiyagd4FC0HEJ5iWQ0KTyHRG23"

rtweet_app(BEARER_TOKEN)

auth_setup_default()

library(rtweet)
auth <- rtweet_app(BEARER_TOKEN)

BEARER_TOKEN
API_KEY_SECRET


# Amigos do Lula

lula_fds <- rtweet::get_friends("LulaOficial", n=Inf)
lula_fds_data <- lookup_users(lula_fds$to_id)

lula_fds_data %>% class()
glimpse(lula_fds_data)

lula_fds_data %>% 
  filter(protected==1) -> lula_fds_locked_accounts
