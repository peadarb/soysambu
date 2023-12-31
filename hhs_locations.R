#locations
library(mapview)
library(sf)
library(dplyr)
library(readxl)

hhs_locations <- read_xlsx("soysambu_data_27112023.xlsx") %>% 
hhs_locations <- read_xlsx("soysambu_data_30112023.xlsx") %>% 
  select("_97. Location (GPS)_latitude", "_97. Location (GPS)_longitude", "1. Name of Enumerator", "17. What is the household heads' village name?") %>% 
  rename("y" = "_97. Location (GPS)_latitude", "x" = "_97. Location (GPS)_longitude", "name" = "1. Name of Enumerator", "village" = "17. What is the household heads' village name?") %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)
mapview::mapview(hhs_locations, zcol = "name")
mapview::mapview(hhs_locations, zcol = "village")
