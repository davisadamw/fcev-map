library(tidyverse)

# take a single shortest path and extract edge attributes
extract_lengths <- function(from_point, all_points, sp_epaths, edge_attrs) {
  tibble(from = from_point, to = all_points) %>% 
    # basically just grabs the edge indices as a vector
    mutate(edges = map(sp_epaths, unclass)) %>% 
    # make into big long df with all edges in each shortest path
    unnest() %>% 
    # ... could move everything below here outside, but that doesn't appear to save any time
    # ... possibly b.c of increase in number of groups for summaries run over multiple FROMs?
    # attach path attributes by index
    bind_cols(slice(edge_attrs, pull(., edges))) %>% 
    group_by(from, to) %>% 
    # calculate total length and carpool along each path
    summarise(tot_length = sum(seg_length),
              carpool_length = sum(carpool_length)) %>% 
    ungroup()
}

# run shortest path and extract relevant lengths
run_and_extract <- function(from_point, all_points, network, edge_attrs) {
  # first, calculate shortest paths
  shortest_paths <- igraph::shortest_paths(network, from = from_point, to = all_points,
                                           mode = 'out', output = 'epath')
  
  extract_lengths(from_point, all_points, pluck(shortest_paths, 'epath'), edge_attrs)
  
}

# build network subset, extract shortest paths, grab capool length and toll indicator
grab_run_extract <- function(from_point, all_points, full_network,
                             x_cent, y_cent, radius) {
  
  # grab the subset network
  network_atts <- full_network %>% 
    subset_circle(x_cent  = x_cent, y_cent  = y_cent, radius  = radius,
                  x1col = X_first, x2col = X_last, y1col = Y_first, y2col = Y_last)
  
  # build igraph from road network
  network_net <- network_atts %>% 
    select(., from, to, weight) %>% 
    igraph::graph_from_data_frame(directed = TRUE)
  
  # identify any missing vertices
  all_points_use <- all_points[all_points %in% names(igraph::V(network_net))]
  
  # if either no destination points remain or source point not in network, return an empty tibble
  if (length(all_points_use) == 0) return(tibble())
  if (!(from_point %in% names(igraph::V(network_net)))) return(tibble())
  
  # run shortest paths
  shortest_paths <- igraph::shortest_paths(network_net, from = from_point, to = all_points_use,
                                           mode = 'out', output = 'epath')
  # extract relevant information
  tibble(from = from_point, to = all_points_use) %>% 
    # basically just grabs the edge indices as a vector
    mutate(edges = map(pluck(shortest_paths, 'epath'), unclass)) %>% 
    # make into big long df with all edges in each shortest path
    unnest(cols = edges) %>% 
    # ... could move everything below here outside, but that doesn't appear to save any time
    # ... possibly b.c of increase in number of groups for summaries run over multiple FROMs?
    # attach path attributes by index
    bind_cols(slice(network_atts, pull(., edges))) %>% 
    group_by(from, to) %>% 
    # calculate total length and carpool along each path
    summarise(tot_length     = sum(seg_length),
              carpool_length = sum(carpool_length),
              any_bridges    = any(bridge2)) %>% 
    ungroup()
  
}


subset_boxbox <- function(dataset, x_range, y_range, xmincol, xmaxcol, ymincol, ymaxcol, buffer=0) {
  
  xmincol <- enquo(xmincol)
  xmaxcol <- enquo(xmaxcol)
  ymincol <- enquo(ymincol)
  ymaxcol <- enquo(ymaxcol)
  
  x_range_min <- x_range[1] - buffer
  x_range_max <- x_range[2] + buffer
  y_range_min <- y_range[1] - buffer
  y_range_max <- y_range[2] + buffer
  
  # to intersect two bounding boxes
  # for each dimension the minimum of one must be less than or equal to the maximum of the other
  # and vice versa (max1 >= min2)
  dataset %>% filter(!!xmincol <= x_range_max,
                     !!xmaxcol >= x_range_min,
                     !!ymincol <= y_range_max,
                     !!ymaxcol >= y_range_min)
  
}


subset_circle <- function(dataset, x_cent, y_cent, radius, x1col, x2col, y1col, y2col) {
  
  x1col <- enquo(x1col)
  x2col <- enquo(x2col)
  y1col <- enquo(y1col)
  y2col <- enquo(y2col)
  
  # basically we're just checking if dX^2 + dY^2 <= Radius^2
  radius_sq <- radius^2
  
  # circle is ~ maximum road distance allowable from source
  # since road distance >= euclidean distance, a euclidean distance filter on the road endpoints 
  # will catch all nodes possibly in range
  dataset %>% filter((!!x1col - x_cent)^2 + (!!y1col - y_cent)^2 <= radius_sq,
                     (!!x2col - x_cent)^2 + (!!y2col - y_cent)^2 <= radius_sq)
  
}


# mehhhhh i should probs separate out these things
grab_local_network <- function(targ_centroid,
                               all_centroids, road_network, 
                               k_neighbors,
                               cent_id_col,
                               X_first_col, X_last_col, Y_first_col, Y_last_col, 
                               from_node_col, to_node_col, weight_col) {
  
  # grab the variables
  cent_id_col   <- enquo(cent_id_col)
  X_first_col   <- enquo(X_first_col)
  X_last_col    <- enquo(X_last_col)
  Y_first_col   <- enquo(Y_first_col)
  Y_last_col    <- enquo(Y_last_col) 
  from_node_col <- enquo(from_node_col)
  to_node_col   <- enquo(to_node_col)
  weight_col    <- enquo(weight_col)
  
  # grab target block group
  one_centroid <- all_centroids %>% filter(!!cent_id_col == targ_centroid)
  
  k_nearest <- RANN::nn2(select(all_centroids, X, Y),
                         select(one_centroid,  X, Y),
                         k = k_neighbors + 1)
  
  neighb_info <- all_centroids %>% 
    # grab the info about nearest neighbor block groups
    select(!!cent_id_col, X_to = X, Y_to = Y) %>% 
    slice(pluck(k_nearest, 'nn.idx')) %>% 
    # grab distance from each block group too
    mutate(distance = pluck(k_nearest, 'nn.dists') %>% as.vector()) %>% 
    # and drop the same location bgs (probably just the original block group)
    filter(distance > 0)
  
  # get max distance to neighbors
  max_dist <- max(neighb_info$distance)
  
  # subset network with the circle
  sub_att <- road_network %>% 
    subset_circle(one_centroid$X, one_centroid$Y,
                  max_dist*1.2, !!X_first_col, !!X_last_col, !!Y_first_col, !!Y_last_col) 
  
  sub_net <- sub_att %>% 
    select(from = !!from_node_col, to = !!to_node_col, weight = !!weight_col) %>% 
    igraph::graph_from_data_frame(directed = TRUE)
  
  return(list(source_name = targ_centroid,
              neighbors   = neighb_info,
              network     = sub_net,
              road_atts   = sub_att))
}

# run local neighbors and just get the % returning NAs
run_network_check <- function(source_name, neighbors, local_network, net_atts, 
                              cent_id_col) {
  
  cent_id_col <- enquo(cent_id_col)
  
  neighbor_ids <- neighbors %>% pull(!!cent_id_col)
  neighbor_ids_use <- neighbor_ids[neighbor_ids %in% names(igraph::V(local_network))]
  
    # first, make sure all the geoid's you have 
  if (length(neighbor_ids_use) < length(neighbor_ids)) {
    warning('uh oh, some vertices missing')
  }
  
  # now, grab all the routes in network
  suppressWarnings(sps <- get_shortest_paths(source_name,
                                             neighbor_ids_use,
                                             local_network,
                                             net_atts))
  
  # how many pairs are missing distances
  return(length(neighbor_ids) - nrow(distinct(sps, to_geoid)))
}
