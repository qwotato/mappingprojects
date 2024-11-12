library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)


cook_county <- get_decennial(
  geography = "tract",
  state = "IL",
  county = "Cook",
  variables = c(
    Population = "P2_001N",
    hispanic = "P2_002N"
  ),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(percent = 100 * (value / summary_value))

# remove empty geometry
cook_county <- cook_county %>% filter(!st_is_empty(cook_county))



tm_shape(cook_county) + 
  tm_polygons()


tm_shape(cook_county) + 
  tm_polygons(col = "summary_value")




