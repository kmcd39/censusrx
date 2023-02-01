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
  ,year = 2021
)

ctimes %>%
  filter(geoid %in% '37055')
