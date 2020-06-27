library(tidyverse)
library(sf)

# distance threshold ~ less than half of vehicle range
d_thresh <- 312/2*0.9*1608

distances <- read_rds("Data/all_station_bg_dists.rds")

# block group # of stations in range ... note: only geoid with two stations has two stations in same place
bg_access <- distances %>%
  with_groups(T_GEOID, summarize, 
              any_05mi = any(d_meters < 5 * 1608, na.rm = TRUE),
              any_10mi = any(d_meters < 10 * 1608, na.rm = TRUE),
              stations = sum(d_meters < d_thresh, na.rm = TRUE))

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
  st_drop_geometry() %>% 
  as_tibble()

# id bad locations
bg_locations_bad  <- anti_join(intpts, bg_access, by = c("GEOID" = "T_GEOID"))
bg_locations_good <- anti_join(intpts, bg_locations_bad, by = "GEOID")

# find nearest good to each bad
nearest_good_geoid <- RANN::nn2(select(bg_locations_good, X, Y),
                                select(bg_locations_bad,  X, Y),
                                k = 1) %>% 
  map(~ .[, 1]) %>% 
  as_tibble()

# don't keep nearest neighbor match if it's at least 10km from its neighbor
bg_bad_matched <- bg_locations_bad %>% 
  bind_cols(nearest_good_geoid) %>% 
  mutate(near_geoid = if_else(nn.dists < 20000,
                              bg_locations_good$GEOID[nn.idx],
                              NA_character_)) %>% 
  left_join(bg_access, c("near_geoid" = "T_GEOID")) %>% 
  select(T_GEOID = GEOID, stations, any_05mi, any_10mi) %>% 
  mutate(across(-T_GEOID, replace_na, 0))

# all block groups
bg_locations %>% 
  select(GEOID, county, geometry) %>% 
  left_join(bind_rows(bg_access, bg_bad_matched), by = c("GEOID" = "T_GEOID")) %>%
  write_sf("Data/Spatial/bg_totals.shp")
