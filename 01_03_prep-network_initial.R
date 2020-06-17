library(tidyverse)
library(sf)
#library(igraph) # tis masks A TON of potentially useful packages, so maybe avoid it and call the 3ish igraph packages directly

source('99_utils.R')
source('99_network-prep-functions.R')

speed_table <- tribble(~highway,        ~speed_miles_ph,
                       'motorway',       60,  
                       'motorway_link',  30,
                       'primary',        45,
                       'primary_link',   30, 
                       'secondary',      30, 
                       'secondary_link', 30, 
                       'tertiary',       30,
                       'tertiary_link',  30, 
                       'trunk',          60, 
                       'trunk_link',     30) %>% 
  mutate(speed_meters_ps = speed_miles_ph * 1609.34 / 3600)


# full network (sf dataset, but lose geometry after extracting bounding box for each)
roads_orig <- read_rds('Data/ca_roads_cleaned_bridge.rds') %>% 
  select(parent_edge, highway, oneway, hov_lane, osm_id_from, osm_id_to, road_length, bridge) %>% 
  mutate(oneway2 = if_else(is.na(oneway), FALSE, oneway == 'yes'),
         bridge2 = if_else(is.na(bridge), FALSE, TRUE))


# get bounding box for each road
roads_fls <- roads_orig %>% 
  object_firstlast()

roads_cleaned <- roads_orig %>% 
  st_drop_geometry() %>% 
  bind_cols(roads_fls)

# points on network ... use nearest neighbor subsample point
t_points_position <- read_rds('Data/station_position.rds') %>%
  select(parent_edge, name, dist_from, dist_to, X = X_nn, Y = Y_nn)

# break roads at BG points and then recombine
roads_allsegs <- break_roads_recombine(roads_cleaned, t_points_position, 
                                       parent_edge, name, 
                                       road_length,
                                       dist_from, dist_to,
                                       osm_id_from, osm_id_to, 
                                       X, Y) %>% 
  select(parent_edge:osm_id_to, bridge, oneway2, bridge2, X_first:seg_length) %>% 
  assign_weights(speed_table, seg_length, speed_meters_ps) %>% 
  select(-speed_miles_ph, -speed_meters_ps)

# and then do the reverse two ways
roads_bothways <- reverse_2way_roads(roads_allsegs, oneway2, force_pst_twoway = FALSE)

roads_bothways %>% write_rds('Data/ca_network.rds', compress = 'gz')

# just for kicks ... let's also try one that only allows one-wayness on motorways, trunks, and linkers
roads_bothways_pst <- reverse_2way_roads(roads_allsegs, oneway2, force_pst_twoway = TRUE)
roads_bothways_pst %>% write_rds('Data/ca_network_pstforced.rds', compress = 'gz')


