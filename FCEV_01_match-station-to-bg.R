library(tidyverse)

# load stations
stations <- read_rds("Data/stations_sf.rds") %>% 
  sf::st_drop_geometry()

# load block groups with positions
bg_pos <- read_rds("Data/ca_bg_seg_position.rds")

# get nearest neighbor
neighbors <- RANN::nn2(select(bg_pos, X_bgcent, Y_bgcent), select(stations, X, Y), k = 1)

stations_bg <- stations %>% 
  mutate(nearest_bg = bg_pos$GEOID[neighbors$nn.idx])

stations_bg %>% 
  write_rds("Data/stations_with_bg.rds") %>% 
  write_csv("Data/stations_with_bg.csv")
