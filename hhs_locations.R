#locations
library(mapview)
library(sf)
library(dplyr)
library(readxl)
library(readr)
library(leaflet)

###########################################################################################

# import shp files -----------------------------------------------------------


s <- read_sf("buildings_sampling/soysambu_shp.shp")
adm <- read_sf("buildings_sampling/KEN_adm5.shp") %>% 
  filter(NAME_2 %in% "Nakuru") %>% 
  filter(NAME_5 %in% c("Lake Nakuru National Park", "Elementaita", "Kiambogo", "Mbaruk", "Eburru"))

mbaruk <- adm %>% 
  filter(NAME_5 == "Mbaruk")
eburru <- adm %>% 
  filter(NAME_5 == "Eburru")
kiambogo <- adm %>% 
  filter(NAME_5 == "Kiambogo")
elementaita <- adm %>% 
  filter(NAME_5 == "Elementaita")
nakuru_np <- adm %>% 
  filter(NAME_5 == "Lake Nakuru National Park")

###########################################################################################

# prepare data -----------------------------------------------------------


#individual locations
hhs_locations <- readRDS("hhs_cleaned_wealth.rds") %>% 
  select("locat", "locatn_lat", "locatn_long", "enum", "village", "livelihood_activity1", "livelihood_activity2", "quintiles") %>% 
  rename("y" = "locatn_lat", "x" = "locatn_long") %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  mutate(   
    livelihood_activity1 = case_when(
      livelihood_activity1 == "Cultivation" ~ "Cultivation",
      livelihood_activity1 == "Kibarua or short-term employment (includes working for someone as bodaboda driver or herder)" ~ "Kibarua",
      livelihood_activity1 == "Own business" ~ "Other",
      livelihood_activity1 == "Food aid" ~ "Other",
      livelihood_activity1 == "Tourism related employment" ~ "Other",
      livelihood_activity1 == "Permanent Employment" ~ "Other",
      livelihood_activity1 == "Livestock and related products" ~ "Livestock",
      livelihood_activity1 == "I do not want to answer" ~ NA)) %>% 
  mutate(   
    livelihood_activity2 = case_when(
      livelihood_activity2 == "Cultivation" ~ "Cultivation",
      livelihood_activity2 == "Kibarua or short-term employment (includes working for someone as bodaboda driver or herder)" ~ "Kibarua",
      livelihood_activity2 == "Own business" ~ "Other",
      livelihood_activity2 == "Food aid" ~ "Other",
      livelihood_activity2 == "Tourism related employment" ~ "Other",
      livelihood_activity2 == "Permanent Employment" ~ "Other",
      livelihood_activity2 == "Livestock and related products" ~ "Livestock",
      livelihood_activity2 == "I do not want to answer" ~ NA)) %>% 
  drop_na(livelihood_activity2) %>% 
  drop_na(livelihood_activity1)

###########################################################################################

# sampling by village -----------------------------------------------------------

hhs_locations_village <- readRDS("hhs_cleaned.rds") %>% 
  select("locat", "locatn_lat", "locatn_long", "enum", "village", "livelihood_activity1", "livelihood_activity2") %>% 
  rename("y" = "locatn_lat", "x" = "locatn_long")

hhs_location1 <- hhs_locations_village %>%
  group_by(locat) %>%
  summarize(num_samples = n())
hhs_location2 <- hhs_locations_village %>%
  group_by(locat) %>%
  summarize(mean_latitude = mean(y),
            mean_longitude = mean(x))
hhs_location <- merge(hhs_location1, hhs_location2, by = 'locat')
my_palette <- c("#202C39", "#d77e5e", "#3d5919", "#381D2A"
                #"#e6e7e2","#a4b792",  "#000000","#202C39", "#d77e5e"
                )

distinct_colors <- my_palette(length(unique(hhs_location$locat)))
map <- leaflet(data = hhs_location) %>%
  addTiles()
map <- map %>%
  addCircleMarkers(
    lat = ~mean_latitude,
    lng = ~mean_longitude,
    radius = ~sqrt(num_samples) * 5,
    color = ~my_palette,
    fillOpacity = 0.7
  ) %>%
  addLegend("bottomright", 
            colors = my_palette,
            labels = unique(hhs_location$locat),
            opacity = 1)
print(map)

###########################################################################################

# livelihoods 1 -----------------------------------------------------------


#my_palette <- c("#a4b792", "#202C39", "#d77e5e", "#3d5919", "#e6e7e2", "#381D2A", "#000000", "#202C39", "#d77e5e")
pal <- colorFactor(c("#3d5919", "#202C39", "#d77e5e","#a4b792"), domain = c("Cultivation", "Kibarua", "Other", "Livestock"))

# Create leaflet map
map <- leaflet(data = hhs_locations) %>%
  addTiles() %>%
  addPolygons(
    fillColor = "darkgreen",
    fillOpacity = 0.3,
    data = s
    ) %>%
  addPolygons(
    fillColor = "transparent",
    color = "black",
    weight = 2,
    data = eburru
  ) %>%
  addPolygons(
    fillColor = "transparent",
    color = "black",
    weight = 2,
    data = elementaita
  )%>%
  addPolygons(
    fillColor = "transparent",
    color = "black",
    weight = 2,
    data = kiambogo
  )%>%
  addPolygons(
    fillColor = "transparent",
    color = "black",
    weight = 2,
    data = mbaruk
  )%>% 
  addCircleMarkers(
    color = ~pal(livelihood_activity1),
    fillOpacity = 0.7
  ) %>%
  addLegend(
    "bottomright", 
    pal,
    values = ~livelihood_activity1,
    opacity = 0.7,
    title = "Principle Livelihood"
  )

# Print the map
print(map)

###########################################################################################

# livelihoods 2 -----------------------------------------------------------


# Create leaflet map
map <- leaflet(data = hhs_locations) %>%
  addTiles() %>%
  addPolygons(
    fillColor = "darkgreen",
    fillOpacity = 0.3,
    data = s
  ) %>%
  addPolygons(
    fillColor = "transparent",
    color = "black",
    weight = 2,
    data = eburru
  ) %>%
  addPolygons(
    fillColor = "transparent",
    color = "black",
    weight = 2,
    data = elementaita
  )%>%
  addPolygons(
    fillColor = "transparent",
    color = "black",
    weight = 2,
    data = kiambogo
  )%>%
  addPolygons(
    fillColor = "transparent",
    color = "black",
    weight = 2,
    data = mbaruk
  )%>% 
  addCircleMarkers(
    color = ~pal(livelihood_activity2),
    fillOpacity = 0.7
  ) %>%
  addLegend(
    "bottomright", 
    pal,
    values = ~livelihood_activity2,
    opacity = 0.7,
    title = "Second Livelihood"
  )

# Print the map
print(map)

###########################################################################################

# map of wealth quintiles -----------------------------------------------------------

#my_palette <- c("#a4b792", "#202C39", "#d77e5e", "#3d5919", "#e6e7e2", "#381D2A", "#000000", "#202C39", "#d77e5e")
pal <- colorFactor(c("#000004", "#160b39", "#420a68", "#6a176e", "#932667", "#ba3655", "#db5858", "#ed7953", "#fb9f3a", "#fdca26", "#f0f921"), 
                   domain = c(1, 2, 3, 4, 5))

hhs_wealth <- hhs_locations %>% 
  drop_na(quintiles)
# Create leaflet map
map <- leaflet(data = hhs_wealth) %>%
  addTiles() %>%
  addPolygons(
    fillColor = "darkgreen",
    fillOpacity = 0.3,
    data = s
  ) %>%
  addPolygons(
    fillColor = "transparent",
    color = "black",
    weight = 2,
    data = eburru
  ) %>%
  addPolygons(
    fillColor = "transparent",
    color = "black",
    weight = 2,
    data = elementaita
  )%>%
  addPolygons(
    fillColor = "transparent",
    color = "black",
    weight = 2,
    data = kiambogo
  )%>%
  addPolygons(
    fillColor = "transparent",
    color = "black",
    weight = 2,
    data = mbaruk
  )%>% 
  addCircleMarkers(
    color = ~pal(quintiles),
    fillOpacity = 0.7
  ) %>%
  addLegend(
    "bottomright", 
    pal,
    values = ~quintiles,
    opacity = 0.7,
    title = "Wealth Quintile (higher is wealthier)"
  )

# Print the map
print(map)
