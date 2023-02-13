library(tidyverse)
library(sf)
library(mapview)

rm(list = ls())
options(tigris_use_cache = TRUE)

devtools::load_all()

# params ------------------------------------------------------------------


state <- 37
cofps <- c('053', '055')
yr <- 2021


# test --------------------------------------------------------------------


ctimes <- commute.times.by.mode(
  geo = 'county'
  ,state = 37
  ,year = yr
)

ctimes %>%
  filter(geoid %in% '37001')

# NA aggregate commute time for Dare county
tidycensus::get_acs(
  geography = 'county'
  ,table = 'B08136'
  ,year = 2021
  ,county = '055'
  ,state = 37
  ,cache_table = T
)


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
