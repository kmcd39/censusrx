library(tidyverse)


# notes -------------------------------------------------------------------

#' just a tibble to map between tigris functions to pull geographies and
#' geographic labels used in tidycensus/ census api
#'
#' https://walker-data.com/tidycensus/articles/basic-usage.html#geography-in-tidycensus
tidycensus::acs5_geography$geography %>% unique()

api.geos <- c(
  "block", "block group", "tract",
  "county", "county subdivision",
  "place", "cbsa", "combined statistical area",
  "urban area", "state"
  )

tigris.fcns <- c(
  tigris::blocks, tigris::block_groups, tigris::tracts
  ,tigris::counties, tigris::county_subdivisions,
  tigris::places, tigris::core_based_statistical_areas, tigris::combined_statistical_areas
  ,tigris::urban_areas, tigris::states
)

tigris.acs.geo.equivalents <-
  tibble(
    api.geos = api.geos
    ,tigris.fcns = tigris.fcns
    )

generic.fcn <- tigris.acs.geo.equivalents[
  tigris.acs.geo.equivalents$api.geos == "block group"
                           , ]$tigris.fcns[[1]]

generic.fcn(state = "25")

# write -------------------------------------------------------------------

usethis::use_data(tigris.acs.geo.equivalents, overwrite = TRUE)
