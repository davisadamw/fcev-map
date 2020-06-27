library(tidyverse)

stations  <- read_rds("Data/stations_with_bg.rds") %>% 
  filter(name != "Emeryville")
distances <- read_rds("Data/all_station_bg_dists.rds")

# only want distances between block groups that have fueling stations
distances_sta <- distances %>% 
  semi_join(stations, by = c("T_GEOID" = "nearest_bg"))

# get nearest three relevant bgs for each bg with a station, compare to euclidean distance
stations_xy <- stations %>% 
  select(id, name, address, nearest_bg, X, Y)


potential_matches <- distances_sta %>% 
  left_join(stations_xy, c("STA_GEOID" = "nearest_bg")) %>% 
  left_join(stations_xy, c("T_GEOID" = "nearest_bg"), suffix = c("", "_nearest")) %>% 
  filter(id != id_nearest) %>% 
  with_groups(id, slice_min, d_meters, n = 1) %>% 
  mutate(d_meters_euclidean = sqrt((X - X_nearest)^2 + (Y - Y_nearest)^2),
         d_meters = pmax(d_meters_euclidean, d_meters)) %>% 
  select(-d_meters_euclidean)

nearest_sta <- potential_matches %>% 
  select(id, name, address, d_meters, id_nearest, name_nearest, address_nearest)

nearest_sta %>% 
  write_csv("Data/fcev_station_to_nearest_station.csv")

