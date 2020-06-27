library(tidyverse)
library(sf)

# distance threshold ~ less than half of vehicle range
d_thresh <- 312/2*0.9*1608

distances <- read_rds("Data/all_station_bg_dists.rds")

# block group # of stations in range ... note: only geoid with two stations has two stations in same place
bg_access <- distances %>%
  with_groups(T_GEOID, summarize, stations = sum(d_meters < d_thresh))

# now for the spatial part
bg_locations <- read_rds("~/GIS DataBase/CA_land_only/area-land-area-water/out_dataset/ca_bgs_nowater.rds")

# for bgs with missing data, update with nearest neighbors
bg_locations_nospatial <- bg_locations %>% 
  st_drop_geometry()

intpts <- bg_locations %>% 
  st_drop_geometry() %>% 
  select(GEOID, INTPTLON, INTPTLAT) %>% 
  st_as_sf(coords = c("INTPTLON", "INTPTLAT"), crs = 4269) %>% 
  st_transform(3310) %>% 
  bind_cols(as_tibble(st_coordinates(.))) %>% 
  st_drop_geometry()

# id bad locations
bg_locations_bad  <- anti_join(intpts, bg_access, by = c("GEOID" = "T_GEOID"))
bg_locations_good <- anti_join(intpts, bg_locations_bad, by = "GEOID")

# find nearest good to each bad
nearest_good_geoid <- RANN::nn2(select(bg_locations_good, X, Y),
                                select(bg_locations_bad,  X, Y),
                                k = 1) %>% 
  pluck("nn.idx") %>% 
  as.vector()

bg_bad_matched <- bg_locations_bad %>% 
  mutate(near_geoid = bg_locations_good$GEOID[nearest_good_geoid]) %>% 
  left_join(bg_access, c("near_geoid" = "T_GEOID")) %>% 
  select(T_GEOID = GEOID, stations)

# all block groups
bg_locations %>% 
  select(GEOID, county, geometry) %>% 
  left_join(bind_rows(bg_access, bg_bad_matched), by = c("GEOID" = "T_GEOID")) %>%
  mutate(stations_na0 = if_else(stations == 0, NA_integer_, stations)) %>% 
  write_sf("Data/Spatial/bg_totals.shp")
