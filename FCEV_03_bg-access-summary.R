library(tidyverse)
library(sf)

# distance threshold ~ less than half of vehicle range
d_thresh <- 312/2*0.9*1608

distances <- read_rds("Data/all_station_bg_dists.rds")

# block group # of stations in range ... note: only geoid with two stations has two stations in same place
bg_access <- distances %>%
  with_groups(T_GEOID, summarize, 
              nearest_sta_miles = min(d_meters) / 1608,
              second_nearest_sta_miles = (sort(d_meters)[2]) / 1608,
              stas_05mi = sum(d_meters < 5 * 1608, na.rm = TRUE),
              stas_10mi = sum(d_meters < 10 * 1608, na.rm = TRUE),
              stas_20mi = sum(d_meters < 20 * 1608, na.rm = TRUE),
              stas_55mi = sum(d_meters < 55 * 1608, na.rm = TRUE),
              stations = sum(d_meters < d_thresh, na.rm = TRUE)) %>% 
  mutate(bg_sta1d = case_when(stas_05mi >= 1 ~ "Nearest station within 5 miles",
                              stas_10mi >= 1 ~ "Nearest station 5-10 miles away",
                              stas_20mi >= 1 ~ "Nearest station 10-20 miles away",
                              stas_55mi >= 1 ~ "Nearest station 20-55 miles away",
                              TRUE ~ "No stations within 55 miles"),
         bg_sta2d = case_when(stas_05mi >= 2 ~ "Second-nearest station within 5 miles",
                              stas_10mi >= 2 ~ "Second-nearest station 5-10 miles away",
                              stas_20mi >= 2 ~ "Second-nearest station 10-20 miles away",
                              stas_55mi >= 2 ~ "Second-nearest station 20-55 miles away",
                              stas_55mi >= 1 ~ "One station within 55 miles",
                              TRUE ~ "No stations within 55 miles"))

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
  select(T_GEOID = GEOID, stations, bg_sta1d, bg_sta2d) %>% 
  mutate(across(-T_GEOID, replace_na, 0))

# add DAC info /// just grab the target tracts
dac_criteria <- readxl::read_excel("Data/sb535dacsces32018update.xlsx",
                                   sheet = "SB535 list (2018 update)",
                                   col_types = "text",
                                   na = c("", "NA")) %>% 
  mutate(GEOID_tr = paste0("0", `Census Tract`)) %>% 
  select(GEOID_tr, dac_status = 3)


# all block groups
bg_location_summary <- bg_locations %>% 
  mutate(GEOID_tr = paste0(STATEFP, COUNTYFP, TRACTCE)) %>% 
  left_join(dac_criteria, by = "GEOID_tr") %>% 
  select(GEOID, county, dac_status, geometry) %>% 
  mutate(dac_status = replace_na(dac_status, "not DAC status")) %>% 
  left_join(bind_rows(bg_access, bg_bad_matched), by = c("GEOID" = "T_GEOID"))


bg_location_summary %>%
  select(GEOID, county, bg_sta1d, bg_sta2d,
         nearest_sta_miles, second_nearest_sta_miles, geometry) %>% 
  write_sf("Data/Spatial/bg_totals_20210920.shp")

bg_location_summary %>% 
  select(-county) %>% 
  st_drop_geometry() %>% 
  write_csv("Data/block_group_to_hydrogen_fueling_station.csv")


