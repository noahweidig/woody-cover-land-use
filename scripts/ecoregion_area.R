pacman::p_load(tidyverse, sf)

# Load the data
etf <- st_read("D:/us_eco_l3/us_eco_l3.shp") %>%
  filter(NA_L1CODE == "8") %>%
  # merge polygons by US_L3NAME
  group_by(US_L3NAME) %>%
  summarise(geometry = st_union(geometry)) %>%
  # add a column for area in hectares
  mutate(area_total_eco = as.numeric(st_area(geometry) / 10^4))
