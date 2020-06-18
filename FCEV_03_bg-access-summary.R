library(tidyverse)
library(sf)

# distance threshold ~ less than half of vehicle range
d_thresh <- 312/2*0.9*1608

distances <- read_rds("Data/all_station_bg_dists.rds") %>% 
  filter(d_meters < d_thresh)

# block group # of stations in range ... note: only geoid with two stations has two stations in same place
bg_access <- distances %>% 
  with_groups(T_GEOID, summarize, stations = n())

bg_locations <- read_rds("~/GIS DataBase/CA_land_only/area-land-area-water/out_dataset/ca_bgs_nowater.rds")

# attach info to block groups and rewrite for mapping
bg_locations %>% 
  left_join(bg_access, by = c("GEOID" = "T_GEOID")) %>% 
  write_sf("Data/Spatial/bg_totals.shp")
