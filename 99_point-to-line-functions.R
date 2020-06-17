library(tidyverse)
library(sf)

source('99_utils.R')

# first an overall function to join locations in a set of points to a set of lines, 
# including the position on the lines 

# points and roads_matchable should both be sf tibbles
join_points_to_roads <- function(points, roads_matchable, spacing,
                                 edge_col, from_col, to_col, length_col) {
  
  # grab field names
  edge_col   <- enquo(edge_col)
  from_col   <- enquo(from_col)
  to_col     <- enquo(to_col)
  length_col <- enquo(length_col)
  
  # find nearest road segment (pull its index)
  nearest_indices <- st_nearest_feature(points, roads_matchable)
  
  # attach info about nearest segment, drop all geom cols
  t_points_nearest_seg <- bind_cols(points %>% 
                                      mutate(X = st_coordinates(.)[,1] %>% unname(),
                                             Y = st_coordinates(.)[,2] %>% unname()) %>% 
                                      st_drop_geometry(),
                                    roads_matchable %>% 
                                      st_drop_geometry() %>% 
                                      select(!!edge_col, !!from_col, !!to_col, !!length_col) %>% 
                                      slice(nearest_indices))
  
  # grab the roads that are nearest to at least one point
  roads_anypoints <- roads_matchable %>% slice(unique(nearest_indices))
  
  # make nested tibble with two cols
  # 1. id for road segment
  # 2. listcol of coordinates for points subsampled at 5m interval for each segment in col 1
  # 3. IMPORTANT: drop in final map will demote 1x2 matrix to a vector of length 2, which messes up nearest_neighbor
  t_segs_sampled <- tibble(!!edge_col := pull(roads_anypoints, !!edge_col),
                           samp_points = line_sample_as_listcol(roads_anypoints, spacing))
  
  # now, for each point that want to place along the linem we just need to find the nearest subsample point
  # ... or really just index of nearest subsample point
  # ... st_line_sample starts 1/2 of distance in, so first point is at 2.5m
  # ... distance from point to FROM node is {2.5 + 5 * (index - 1) }
  # ... distance from point to TO   node is {seg_len - distance_to_FROM}
  t_points_position <- t_points_nearest_seg %>% 
    # group all the points on each line
    group_by(!!edge_col, !!from_col, !!to_col, !!length_col) %>% 
    nest() %>% 
    # attach the sample points from each line to the points from that line
    left_join(t_segs_sampled, by = as_label(edge_col)) %>% 
    # get the nearest sample point to each point
    mutate(nns = map2(samp_points, data, ~nearest_neighbor(.x, select(.y, X, Y)))) %>% 
    select(-samp_points) %>% 
    unnest(c(data, nns)) %>% 
    ungroup() %>% 
    mutate(dist_from = (spacing/2) + spacing * (nn_index - 1),
           dist_to   = !!length_col - dist_from) %>% 
    rename(X_bgcent = X, Y_bgcent = Y)
  
  return(t_points_position)
}


# run line sample and return results as a list column
line_sample_as_listcol <- function(roads_anypoints, spacing) {
  st_line_sample(roads_anypoints, density = 1/spacing, type = 'regular') %>% 
    st_coordinates() %>% 
    as_tibble() %>% 
    group_by(L1) %>% 
    nest(coords = c(X, Y)) %>% 
    pull(coords)
}

# function to grab single nearest neighbor, output as a tibble
nearest_neighbor <- function(match_points, targ_points) {
  nn_obj <- RANN::nn2(match_points, targ_points)
  nn_tib <- tibble(nn_index = nn_obj$nn.idx[,1], 
                   nn_dist  = nn_obj$nn.dists[,1]) 
  
  xys <- match_points %>% slice(pull(nn_tib, nn_index))
  
  nn_tib %>% 
    mutate(X_nn = xys %>% pull(1),
           Y_nn = xys %>% pull(2))
}


