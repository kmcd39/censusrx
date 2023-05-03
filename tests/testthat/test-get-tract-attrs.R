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


test_that("cattrs n no cars",
          {expect_equal(
             unique(nchar(bttrs$geoid))
            ,12)
          })


# scratch -----------------------------------------------------------------

# pick function???

df <- tibble(
  x = c(3, 2, 2, 2, 1),
  y = c(0, 2, 1, 1, 4),
  z1 = c("a", "a", "a", "b", "a"),
  z2 = c("c", "d", "d", "a", "c")
)
df

# `pick()` provides a way to select a subset of your columns using
# tidyselect. It returns a data frame.
df %>% mutate(cols = pick(x, y))
