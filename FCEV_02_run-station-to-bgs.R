library(tidyverse)
library(cppRouting)

# load stations
stations_bg <- read_rds("Data/stations_with_bg.rds")

bg_stations <- stations_bg %>% 
  count(nearest_bg)

# load network
network_raw <- read_rds("Data/ca_network_pstforced_updated.rds")

# get ids of all other block groups
geoid_nodes <- network_raw %>% 
  distinct(from_node, to_node) %>% 
  pivot_longer(everything(), names_to = NULL, values_to = "node_id") %>% 
  distinct(node_id) %>% 
  filter(str_starts(node_id, "06") & str_length(node_id) == 12) %>% 
  pull(node_id)

# prep network for routing ... seg_length is in meters
network_cpp <- network_raw %>% 
  select(from_node, to_node, seg_length) %>% 
  makegraph()

# run all stations to all block groups
station_to_blockgroup_mat <- network_cpp %>% 
  get_distance_matrix(bg_stations$nearest_bg,
                      geoid_nodes)

# get distances in a usable format
# finally, to a usable format
distances_tbl <- station_to_blockgroup_mat %>% 
  as_tibble(rownames = 'STA_GEOID') %>% 
  pivot_longer(-STA_GEOID, 
               names_to = 'T_GEOID',
               values_to = 'd_meters')

# save to disk
distances_tbl %>% 
  left_join(bg_stations, by = c("STA_GEOID" = "nearest_bg")) %>% 
  rename(stations = n) %>% 
  write_rds("Data/all_station_bg_dists.rds", compress = "gz") %>% 
  write_rds("Data/all_station_bg_dists.csv")
