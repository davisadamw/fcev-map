library(tidyverse)
source('99_route-extraction-functions.R')
source('99_run-network-by-block-group.R')


# load road network (primary/secondary/tertiary has forced two-way)
network_prepped <- read_rds('Data/ca_network_pstforced.rds')

tract_locs <- read_rds('Data/station_position.rds') %>% 
  select(name, X = X_bgcent, Y = Y_bgcent)

full_metal_wrapper <- function(targ_centroid, all_centroids, road_network, k_neighbors) {
  local_network <- grab_local_network(targ_centroid,
                                      all_centroids, road_network, 
                                      k_neighbors,
                                      name,
                                      X_first, X_last, Y_first, Y_last, 
                                      from_node, to_node, weight)
  
  run_network_check(local_network$source_name,
                    local_network$neighbors,
                    local_network$network,
                    local_network$road_atts,
                    name)
}

# test the network check on a random block group
# local_network <- full_metal_wrapper(sample(block_group_locs$GEOID, 1),
#                                     block_group_locs, network_prepped, 
#                                     50)

# check all block groups to ID the really bad ones
trs_test_all <- tract_locs %>%
  mutate(n_fails = map_int(name, full_metal_wrapper,
                           all_centroids = tract_locs,
                           road_network = network_prepped,
                           k_neighbors  = 10))

# and save the results
trs_test_all %>% write_rds('Data/bad_trs_run1.rds')







