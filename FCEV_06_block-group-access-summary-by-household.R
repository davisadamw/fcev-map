library(tidyverse)
library(tidycensus)

all_vars <- load_variables(2019, "acs5")

# get total block group househodls
bg_households <- get_acs("block group",
                         variables = "B11012_001",
                         state = "CA") %>% 
  select(GEOID, households = estimate)

tot_households <- sum(bg_households$households)

bg_location_summary <- read_csv("Data/block_group_to_hydrogen_fueling_station.csv",
                                col_types = cols_only(
                                  GEOID = col_character(),
                                  nearest_sta_miles = col_number(),
                                  second_nearest_sta_miles = col_number(),
                                  third_nearest_sta_miles = col_number()),
                                lazy = FALSE) %>% 
  rename(sta_1 = nearest_sta_miles,
         sta_2 = second_nearest_sta_miles,
         sta_3 = third_nearest_sta_miles)

bg_households %>% 
  left_join(bg_location_summary, by = "GEOID") %>% 
  pivot_longer(sta_1:sta_3,
               names_to = "station",
               names_prefix = "sta_",
               values_to = "distance") %>% 
  mutate(within = case_when(distance <=  5 ~  5,
                            distance <= 10 ~ 10,
                            distance <= 20 ~ 20,
                            distance <= 55 ~ 55,
                            TRUE           ~ Inf)) %>% 
  with_groups(c(station, within), summarize, 
              households = sum(households)) %>% 
  mutate(fraction = households / tot_households,
         text = glue::glue("{scales::comma(households)} ({scales::percent(fraction, 1)})")) %>% 
  pivot_wider(id_cols = within,
              names_from = station,
              names_prefix = "station_",
              values_from = text) %>% 
  clipr::write_clip()

