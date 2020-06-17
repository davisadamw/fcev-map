library(tidyverse)

# add the new links to the links that didn't have any points
break_roads_recombine <- function(roads_cleaned, points_at_roads, 
                                  road_id_col, point_id_col, 
                                  road_length_col,
                                  dist_from_col, dist_to_col,
                                  from_old_col, to_old_col,
                                  X_col, Y_col) {
  
  road_id_col     <- enquo(road_id_col)
  point_id_col    <- enquo(point_id_col)
  road_length_col <- enquo(road_length_col)
  dist_from_col   <- enquo(dist_from_col)
  dist_to_col     <- enquo(dist_to_col)
  from_old_col    <- enquo(from_old_col)
  to_old_col      <- enquo(to_old_col)
  X_col           <- enquo(X_col)
  Y_col           <- enquo(Y_col)
  
  roads_split <- break_roads_at_points(roads_cleaned, points_at_roads, 
                                       !!road_id_col, !!point_id_col, 
                                       !!dist_from_col, !!dist_to_col,
                                       !!from_old_col, !!to_old_col, 
                                       !!X_col, !!Y_col)
  
  roads_allsegs <- roads_cleaned %>% 
    # remove the roads that were broken into pieces
    anti_join(roads_split, by = 'parent_edge') %>% 
    mutate(seg_id = paste(!!road_id_col, '1', sep = '_')) %>% 
    # rename from node, and to node
    mutate(from_node  = !!from_old_col,
           to_node    = !!to_old_col,
           seg_length = !!road_length_col) %>% 
    bind_rows(roads_split)
  
  return(roads_allsegs)

}

break_roads_at_points <- function(roads_cleaned, points_at_roads, 
                                  road_id_col, point_id_col, 
                                  dist_from_col, dist_to_col,
                                  from_old_col, to_old_col, 
                                  X_col, Y_col) {
  
  road_id_col    <- enquo(road_id_col)
  point_id_col   <- enquo(point_id_col)
  dist_from_col  <- enquo(dist_from_col)
  dist_to_col    <- enquo(dist_to_col)
  from_old_col   <- enquo(from_old_col)
  to_old_col     <- enquo(to_old_col)
  X_col          <- enquo(X_col)
  Y_col          <- enquo(Y_col)
  
  # create the secondary edges on lines with sample points
  roads_with_points <- roads_cleaned %>% 
    inner_join(points_at_roads, by = 'parent_edge') %>% 
    # sort the points by line and position on line
    arrange(!!road_id_col, !!dist_from_col) %>% 
    group_by(!!road_id_col) %>% 
    # calculate resulting subsegment number
    mutate(subseg_no = row_number(),
           is_first  = subseg_no == 1) %>% 
    ungroup()
  
  # define new links along the road (add one to get from last point to TO node)
  roads_split_to_points <- roads_with_points %>% 
    group_by(!!road_id_col) %>% 
    # for each point, an edge is added from the previous point
    # if it's the first point on the line, from = osm_id_from (original from node)
    # road length is equal to the difference in distance from start
    # update last xy
    mutate(# if this is the first segment, keep the original from node, otherwise use the previous GEOID / to node
      from_node   = if_else(is_first, !!from_old_col, lag(!!point_id_col)),
      to_node     = !!point_id_col,
      # dist_from is the position of the GEOID on the road relative to the first intersection
      seg_length  = !!dist_from_col - lag(!!dist_from_col, default = 0),
      # if this is the first segment, keep original XY, otherwise use the previous end XY
      X_first     = if_else(is_first, !!X_col, lag(!!X_col)),
      Y_first     = if_else(is_first, !!Y_col, lag(!!Y_col)),
      # this should be the snapped XY position on the road!!!
      X_last      = !!X_col,
      Y_last      = !!Y_col) %>% 
    ungroup()
  
  # a final edge will be created from the last new point to the TO node
  roads_split_last_edge <- roads_with_points %>% 
    # sort the points by line and position on line
    group_by(!!road_id_col) %>% 
    # grab the last edge
    top_n(1, subseg_no) %>% 
    # set its from_node to be the last new point (current to_node) and length to be dist_to (distance to end)
    # update first xy ... ALSO need to update last XY too danggggg
    mutate(subseg_no   = subseg_no + 1,
           from_node   = !!point_id_col,
           to_node     = !!to_old_col,
           seg_length  = !!dist_to_col,
           X_first     = !!X_col,
           Y_first     = !!Y_col) %>% 
    ungroup()
  
  # combine the start and end segments
  roads_split_all <- bind_rows(roads_split_to_points, roads_split_last_edge) %>% 
    mutate(seg_id = paste(!!road_id_col, subseg_no, sep = '_'))
  
  return(roads_split_all)
  
}



# now to use it as a network, reverse the twoway streets and assign a speed
# switch_fields should be a named list like what you use for the by in join
reverse_2way_roads <- function(roads_allsegs, oneway_col, 
                               fields_reverse = c('from_node' = 'to_node',
                                                  'X_first'   = 'X_last', 
                                                  'Y_first'   = 'Y_last'), 
                               force_pst_twoway = FALSE) {
  
  oneway_col <- enquo(oneway_col)
  
  # first, grab roads to reverse, depending on the settings
  # at very least, exclude oneway streets
  if (force_pst_twoway) {
    roads_to_reverse <- roads_allsegs %>% 
      filter(isFALSE(!!oneway_col) | highway %in% c('primary', 'secondary', 'tertiary'))
  } else {
    roads_to_reverse <- roads_allsegs %>% 
      filter(!(!!oneway_col)) 
  }
  
  roads_reversed <- fields_reverse %>% 
    # for roads that are two-way, switch the from and to nodes, XY coords
    # this'll basically loop over the pairs of fields, switching them
    # reduce will feed it pairs of names to switch and take the dataframe as the first input, 
    # starting with the original dataset
    reduce2(names(.), switch_fields, .init = roads_to_reverse) 
  
  # and reattach the reversed rows 
  return(bind_rows(roads_allsegs, roads_reversed))
  
}

# assign speed values and send to igraph
assign_weights <- function(roads, speed_table, 
                           length_col, speed_col, 
                           conversion = 1/60, roadtype_by = 'highway') {
  
  length_col <- enquo(length_col)
  speed_col  <- enquo(speed_col)
  
  roads %>% 
    left_join(speed_table, by = roadtype_by) %>% 
    # approximate travel time (minutes)
    mutate(weight = !!length_col / !!speed_col * conversion)
}
