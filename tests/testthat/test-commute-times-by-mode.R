library(tidyverse)
library(sf)
library(mapview)

rm(list = ls())
options(tigris_use_cache = TRUE)

devtools::load_all()

# params ------------------------------------------------------------------


state <- 37
cofps <- c('159')
yr <- 2021


# test --------------------------------------------------------------------


ctimes <- commute.times.by.mode(
  geo = 'county'
  ,cofp = cofps
  ,state = 37
  ,year = yr
)

ctimes

# NA aggregate commute time for area
tidycensus::get_acs(
  geography = 'county'
  ,table = 'B08136'
  ,year = 2021
  ,county = cofps
  ,state = state
  ,cache_table = T
)

# but compare for

# compare state pull to single county

# state pull
ctimes %>% filter(geoid %in% '37001')

commute.times.by.mode(
  geo = 'county'
  ,cofps = '001'
  ,state = 37
  ,year = yr
)
#(checks)
