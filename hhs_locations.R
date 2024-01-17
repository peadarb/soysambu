#locations
library(mapview)
library(sf)
library(dplyr)
library(readxl)
library(readr)
library(leaflet)

#individual locations
hhs_locations <- read_rds("hhs_raw.rds") %>% 
  select("locatn_lat", "locatn_long", "enum", "village") %>% 
  rename("y" = "locatn_lat", "x" = "locatn_long") %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)
mapview::mapview(hhs_locations, zcol = "village")

# summarise by village
hhs_agreed <- hhs %>% 
  filter(agreed == "Yes") %>% 
  select(-name_respondent) %>% 
  select(locatn_lat, locatn_long, locat, village)
hhs_location1 <- hhs_agreed %>%
  group_by(village) %>%
  summarize(num_samples = n())
hhs_location2 <- hhs_agreed %>%
  group_by(village) %>%
  summarize(mean_latitude = mean(locatn_lat),
            mean_longitude = mean(locatn_long))
hhs_location <- merge(hhs_location1, hhs_location2, by = 'village')
distinct_colors <- rainbow(length(unique(hhs_location$village)))
map <- leaflet(data = hhs_location) %>%
  addTiles()
map <- map %>%
  addCircleMarkers(
    lat = ~mean_latitude,
    lng = ~mean_longitude,
    radius = ~sqrt(num_samples) * 5,
    color = ~distinct_colors,
    fillOpacity = 0.7
  ) %>%
  addLegend("bottomright", 
            colors = distinct_colors,
            labels = unique(hhs_location$village),
            opacity = 1)
print(map)