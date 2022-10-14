
read_regex <- function(reg, .f = read_csv2){
  
  files <- dir("data/", pattern =   {{reg}}  )
  
  files <- str_c("data/", files)
  
  map_dfr(.x=files, .f=  {{.f}}   , locale = locale(encoding = "ISO-8859-1"))
  
}
