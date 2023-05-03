library(tidyverse)
library(sf)
library(mapview)

rm(list = ls())
options(tigris_use_cache = TRUE)

devtools::load_all()
devtools::document()


# get pulls ---------------------------------------------------------------

cotts <-
  censusrx::gett.census.totals(
     states = '09'
    ,geo = 'county'
    ,years = c(2016, 2021)
  )

cotgrs <- cotts %>%
  pivot_longer(matches('pop|hh|hunits')
               ,values_to = 'estimate'
               ,names_to = 'variable') %>%
  censusrx::get.cagr.comparison() %>%
  arrange(desc(cagr))


# basic test --------------------------------------------------------------------

tmp <- cotts %>%
  filter(geoid == '09001') %>%
  arrange(year)


test_that("manual cagr calc vs fcn", {
  expect_equal( (tmp$pop[2] / tmp$pop[1]) ^ (1/5)  - 1
               ,
               cotgrs %>% filter(geoid == '09001' & variable == 'pop' & year ==2021) %>% .$cagr)
})


# dev renames -------------------------------------------------------------


