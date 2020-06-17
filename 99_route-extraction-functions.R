library(tidyverse)
library(sf)

get_shortest_paths <- function(from_node, to_nodes, network, edge_attrs) {
  # first, calculate shortest paths
  shortest_paths <- igraph::shortest_paths(network, from = from_node, to = to_nodes,
                                           mode = 'out', output = 'epath') %>% 
    pluck('epath')
  
  # then extract edge attributes
  tibble(from_geoid = from_node, to_geoid = to_nodes) %>% 
    # grab the edge indices as a vector
    mutate(edges = map(shortest_paths, unclass)) %>% 
    # make into big long df with all edges in each shortest path
    unnest() %>% 
    # attach path attributes by index ... this relies on network to be ordered the same as edge_attrs
    bind_cols(slice(edge_attrs, pull(., edges)))
}


# function to grab the appropriate edges for a route
grab_route <- function(from, to, edges_in_route, edges_spatial) {
  
  targ_routes <- edges_in_route %>% 
    filter(from_geoid == from,
           to_geoid   == to) %>% 
    pull(parent_edge)
  
  edges_spatial %>% 
    filter(parent_edge %in% targ_routes)
  
}





  
  