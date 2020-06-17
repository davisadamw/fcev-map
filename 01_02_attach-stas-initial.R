library(tidyverse)
library(sf)
source('99_utils.R')
source('99_point-to-line-functions.R')

# spacing distance constant (meters)
spacing <- 1

# load CA block groups, grab internal point
stas <- read_rds('Data/stations_sf.rds')

# load roads, extract only matchable (road type is not motorway or link)
roads_cleaned <- read_rds('Data/ca_roads_cleaned_bridge.rds')
roads_matchable <- roads_cleaned %>% 
  filter(highway != 'motorway', !str_ends(highway, '_link'))

# join bg center points to nearest roads
t_points_position <- join_points_to_roads(stas, roads_matchable, spacing,
                                          edge_col   = parent_edge,
                                          from_col   = osm_id_from,
                                          to_col     = osm_id_to,
                                          length_col = road_length)


t_points_position %>% write_rds('Data/station_position.rds')


# network will then be built from existing edges (both forwards and backwards),
# with edges containing






