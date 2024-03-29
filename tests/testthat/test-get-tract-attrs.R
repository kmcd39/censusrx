library(tidyverse)
library(sf)
library(mapview)

rm(list = ls())
options(tigris_use_cache = TRUE)

devtools::load_all()
devtools::document()

# params & setup ------------------------------------------------------------------

state <- 37
cofps <- c('053', '055')
yr <- 2021

# get metadata
metadata <- censusrx::pull.acs.metadata(year = yr)

# pull tracts -------------------------------------------------------------

devtools::load_all()

attrs <- get.tract.attrs(state = state
                         ,cofps = cofps
                         ,year = yr)
attrs


# # check medians -----------------------------------------------------------

check <- pull.tidycensus.median.tables(state = state
                                       ,cofps = cofps
                                       ,year = yr)

check
names(check)

attrs <- attrs %>% arrange(geoid)
check <- check %>% map( ~arrange(., geoid) )

test_that("cattrs medhhinc",
          {expect_equal(attrs$med.hhinc, check$med.hhinc$n)
            })


# check car ownership -----------------------------------------------------

attrs$nhh.zerocar
attrs$perc.no.car

cown <- tidycensus::get_acs(
  geography = 'tract'
  ,variables = paste0('B08201_00', 1:6)
  ,year = yr
  ,survey = 'acs5'
  ,state = state
  ,county = cofps
) %>%
  select(-NAME) %>%
  rename_with(tolower)

cown <- cown %>%
  left_join(
    select(metadata
           ,variable = name, label)
  ) %>%
  filter(label == 'Total' |
           grepl('No vehicle available', label)) %>%
  select(-label, ) %>%
  pivot_wider(names_from = variable
              ,values_from = c(estimate, moe)) %>%
  arrange(geoid)

test_that("cattrs n no cars",
          {expect_equal(
            attrs$nhh.zerocar
            ,cown$estimate_B08201_002)
          })

test_that("cattrs perc no cars",
          {expect_equal(
            attrs$perc.no.car
            ,cown$estimate_B08201_002 / cown$estimate_B08201_001)
          })


# test w BGs --------------------------------------------------------------

bttrs <- get.tract.attrs(state = state
                         ,cofps = cofps
                         ,year = yr
                         ,geo = 'block group')
attrs$geoid %>% nchar()
attrs %>% nrow()

bttrs$geoid %>% nchar()
bttrs %>% nrow()

test_that("nbhd.attrs.did get block groups when asked?",
          {expect_equal(
             unique(nchar(bttrs$geoid))
            ,12)
          })



## in lf at bg level -------------------------------------------------------

bttrs

check.lf <- tidycensus::get_acs(
  geography = 'block group'
  ,table = 'B23025'
  ,year = yr
  ,survey = 'acs5'
  ,state = state
  ,county = cofps
) %>%
  select(-NAME) %>%
  rename_with(tolower)

check.lf <- check.lf %>%
  select( -moe ) %>%
  pivot_wider(values_from = estimate
              ,names_from = variable) %>%
  mutate( lfpr =
            B23025_002 / B23025_001
          ,unemply.rate =
            B23025_005 / B23025_002
  )

check.lf


test_that("nbhd.attrs.BGs LF check",
          {expect_equal(
             bttrs$lfpr
            ,check.lf$lfpr
            )
          })

test_that("nbhd.attrs.BGs unemployment rate check",
          {expect_equal(
            bttrs$unemply.rate
            ,check.lf$unemply.rate )
          })


# scratch -----------------------------------------------------------------
