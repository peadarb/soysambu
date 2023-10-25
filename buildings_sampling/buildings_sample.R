library(sf)
library(dplyr)

s <- read_sf("buildings_sampling/soysambu_shp.shp")

#import all buildings from 1km buffer
build <- read_sf("buildings_sampling/buildings_within_buffer.geojson")

#import adm area boundaries
adm <- read_sf("buildings_sampling/KEN_adm5.shp") %>% 
  filter(NAME_2 %in% "Nakuru") %>% 
  filter(NAME_5 %in% c("Lake Nakuru National Park", "Elementaita", "Kiambogo", "Mbaruk", "Eburru"))


# Plot the clipped adm dataset
mapview::mapview(s) +
  mapview::mapview(build) +
  mapview::mapview(adm)

# Create a list to store sf layers
admin_areas_sf_list <- setNames(vector("list", nrow(adm)), adm$NAME_5)

# Loop through each area and store buildings within it
for (i in 1:nrow(adm)) {
  adm_area <- adm[i, ]
    # buildings within the area
  buildings_within_area <- st_intersection(build, adm_area)
    # buildings in a list
  admin_areas_sf_list[[i]] <- buildings_within_area
}

mbaruk <- admin_areas_sf_list[["Mbaruk"]]
eburru <- admin_areas_sf_list[["Eburru"]]
kiambogo <- admin_areas_sf_list[["Kiambogo"]]
elementaita <- admin_areas_sf_list[["Elementaita"]]
nakuru_np <- admin_areas_sf_list[["Lake Nakuru National Park"]]

mapview::mapview(s, alpha.regions = 0.1, color = "darkgreen", col.regions = "darkgreen") +
  mapview::mapview(adm, color = "black", alpha.regions = 0, label = "NAME_5") +
  mapview::mapview(mbaruk, lwd = 0.1, col.regions = "red") +
  mapview::mapview(eburru, lwd = 0.1, col.regions = "purple") +
  mapview::mapview(kiambogo, lwd = 0.1, col.regions = "darkblue") +
  mapview::mapview(elementaita, lwd = 0.1, col.regions = "orange") +
  mapview::mapview(nakuru_np, lwd = 0.1, col.regions = "yellow")
  