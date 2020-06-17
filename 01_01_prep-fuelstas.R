library(tidyverse)
library(sf)

# load fueling stations, project
fueling_stations <- read_csv("Data/stations.csv")

fueling_stations_sf <- fueling_stations %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>% 
  st_transform(3310) %>% 
  bind_cols(as_tibble(st_coordinates(.)))

# save block group locations
fueling_stations_sf %>% write_rds('Data/stations_sf.rds')

