library(tidyverse)
library(sf)

# load stations
stations <- read_csv("Data/stations_raw_20210920.csv",
                     col_types = cols_only(`Station Name` = col_character(),
                                           `Open Date`    = col_date(),
                                           Latitude = col_number(),
                                           Longitude = col_number()),
                     lazy = FALSE) %>% 
  janitor::clean_names(case = "snake") %>% 
  rename(name = station_name)

# reproject station points
stations_XY <- stations %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(3310) %>% 
  bind_cols(as_tibble(st_coordinates(.))) %>% 
  st_drop_geometry()

# load block groups with positions
bg_pos <- read_rds("Data/ca_bg_seg_position.rds")

# get nearest neighbor
neighbors <- RANN::nn2(select(bg_pos, X_bgcent, Y_bgcent), select(stations_XY, X, Y), k = 1)

stations_bg <- stations_XY %>% 
  mutate(nearest_bg = bg_pos$GEOID[neighbors$nn.idx])

stations_bg %>% 
  write_rds("Data/stations_with_bg.rds") %>% 
  write_csv("Data/stations_with_bg.csv")
