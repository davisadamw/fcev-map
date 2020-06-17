library(sf)
# wow, this function's great!
# except that it only works with single left, right arguments right now lol
# inspired by: https://stackoverflow.com/a/48450170
# ...'s get passed through to the appropriate join function
join_by_quosure <- function(x, y, by_left, by_right, join_type = 'left', ...){
  
  # name the join types available
  join_types <- paste(c('left', 'right', 'inner', 'semi', 'anti', 'full'), 'join', sep = '_')
  require(rlang, include.only = 'as_name', quietly = TRUE)
  require(dplyr, include.only = join_types, quietly = TRUE)
  
  # sorta awk, since set_names does stuff in reverse
  by <- set_names(as_name(enquo(by_right)), as_name(enquo(by_left)))
  
  # identify the join type, throw error if join_type isn't in the list 
  # (allows partial matching, e.g. 'left' or 'l' for left_join)
  join_use <- match.arg(join_type, join_types)
  
  # and then do the selected join
  return(do.call(join_use, list(x, y, by, ...)))
  
}

# simple function to switch names oftwo fields 
switch_fields <- function(data, v1, v2) {
  v1 = enquo(v1)
  v2 = enquo(v2)
  
  data %>% 
    rename(v0 = !!v1,) %>% 
    rename(!!v1 := !!v2,
           !!v2 := v0)
}

# returns a number of matches for simple is.na or text matching tests for rows by name
# provide a char vector of columns to match by !is.na, a char vector of columns to match by str_detect, and a text string for that
match_mult <- function(data, 
                       any_cols = NULL, 
                       text_cols = NULL,
                       text_match = '.') {
  
  any_cols  <- names(data)[names(data) %in% any_cols]
  text_cols <- names(data)[names(data) %in% text_cols]
  
  any_results  <- map_dfc(any_cols,  ~ tibble(!!. := !is.na(pull(data, .))))
  text_results <- map_dfc(text_cols, ~ tibble(!!. := str_detect(pull(data, .), text_match)))
  
  if (nrow(any_results) == 0) all_results = text_results
  else all_results <- bind_cols(any_results, text_results)
  
  return(rowSums(all_results))
  
}

# function to extract first and last XY point from a polygon or line
object_firstlast <- function(sf_col) {
  st_coordinates(sf_col) %>% 
    as_tibble() %>% 
    group_by(L1) %>% 
    summarise(X_first = first(X),
              Y_first = first(Y),
              X_last  = last(X),
              Y_last  = last(Y)) %>% 
    select(-L1)
}





