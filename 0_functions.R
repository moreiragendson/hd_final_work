
# read data ---------------------------------------------------------------


read_regex <- function(reg, .f = read_csv2){
  
  files <- dir("data/", pattern =   {{reg}}  )
  
  files <- str_c("data/", files)
  
  map_dfr(.x=files, .f=  {{.f}}   , locale = locale(encoding = "ISO-8859-1"))
  
}


# séries históricas -------------------------------------------------------

lab_years <- function(data, year){
  
  var <- data %>% 
    pull({{year}})
  
  seq(  min(var),
        max(var), 1)
  
} 

