library(tidyverse)
library(sf)
library(mapview)

rm(list = ls())
options(tigris_use_cache = TRUE)

devtools::load_all()

# params & setup ------------------------------------------------------------------

state <- 37
cofps <- c('053', '055')
yr <- 2021

# pull metadata
metadata <- pull.acs.metadata(year = yr)


# geos -------------------------------------------------------------------------

ctsf <- tigris::tracts(
  state = state
  ,county = cofps
  ,year = yr
) %>%
  rename_with(tolower) %>%
  select(geoid, aland, geometry)

# check tots --------------------------------------------------------------

ctts <- censusrx::gett.census.totals(
  states = 37
  ,years = 2021
  ,geo = 'county'
  ,cofps = c('055', '053')
)

ctts

check <- tidycensus::get_acs(
  geography = 'county'
  ,year = 2021
  ,state = 37
  ,county =  c('055', '053')
  ,variables = c(pop2 = 'B01001_001')
) %>% rename_with(tolower)

check <- ctts %>%
  left_join(
    check
  )

testthat::expect_equal(
  check$pop, check$estimate
)

# censusrx tblList pull ----------------------------------------------------------

#devtools::load_all()

acl <- censusrx::tidycensus2recoded.tblList(
  states = 37
  ,years = 2021
  ,geo = 'county'
  ,cofps = c('055', '053')
)

names(acl)

acs.demographic.recode

## demographics check ------------------------------------------------------

# sum of demographic groups should be == total pop
check <- acl$B03002 %>%
  group_by(year, geoid) %>%
  summarise(n = sum(n)) %>%
  left_join(ctts)

testthat::expect_equal(
  check$n, check$pop
)

## building age check  ------------------------------------------------------

check <- acl$B25034 %>%
  group_by(year, geoid) %>%
  summarise(n = sum(n)) %>%
  left_join(ctts)

testthat::expect_equal(
  check$n, check$n.hunits
)

## median value checks -----------------------------------------------------

acm <- censusrx::pull.tidycensus.median.tables(
  states = 37
  ,years = 2021
  ,geo = 'tract'
  ,cofps = c('055', '053')
)

names(acm)
acm

check <- tidycensus::get_acs(
  table = 'B19013'
  ,year = yr
  ,state = state
  ,county = cofps
  ,geography = 'tract'
  ,cache_table = T
) %>%
  rename_with(tolower) %>%
  select(-name) %>%
  left_join(metadata[c('name', 'label')]
            , by = c('variable' = 'name'))

check <- check %>%
  left_join(acm$med.hhinc)

# estimate is from tidycensus direct pull above; n is from helper function
testthat::expect_equal(
  check$estimate, check$n
)

# check median monthly housing costs
acm$med.hcosts

check <- tidycensus::get_acs(
  table = 'B25105'
  ,year = yr
  ,state = state
  ,county = cofps
  ,geography = 'tract'
  ,cache_table = T
) %>%
  rename_with(tolower) %>%
  select(-name) %>%
  left_join(metadata[c('name', 'label')]
            , by = c('variable' = 'name'))

check <- check %>%
  left_join(acm$med.hcosts)

# estimate is from tidycensus direct pull above; n is from helper function
testthat::expect_equal(
  check$estimate, check$n
)


# commute shares check ----------------------------------------------------

names(acl)

acl$B08006

check <- tidycensus::get_acs(
  table = 'B08006'
  ,year = yr
  ,state = state
  ,county = cofps
  ,geography = 'tract'
  ,cache_table = T
) %>%
  mutate(var = extract.acs.var(variable)) %>%
  rename_with(tolower) %>%
  select(-name) %>%
  left_join(metadata[c('name', 'label')]
            , by = c('variable' = 'name'))


# there shouldn't be any NAs in recodes or values
#devtools::install_github("kmcd39/taux")

testthat::expect_equal(
  {acl$B08006 %>%
    select(recode, n) %>%
    taux::sum.NAs() %>%
    sum()}, 0
)

# totals from recoded table w aggregates pre-filtered should be equal to total i
# just pulled for check
check <- acl$B08006 %>%
  group_by(year, geoid) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  inner_join(
    filter(check,
           variable == 'B08006_001' # total
    ))

testthat::expect_equal(
  check$n, check$estimate
)


# test with cbsas ---------------------------------------------------------

# devtools::load_all()

cbsapops <- gett.census.totals(
  years = 2021
  ,geo = 'cbsa'
  ,states = NULL
)
cbsapops


check <- tidycensus::get_acs(
   geography = 'cbsa'
  ,year = 2021
  ,variables = c(pop2 = 'B01001_001')
) %>% rename_with(tolower)

check <- cbsapops %>%
  left_join(
  check
)

testthat::expect_equal(
  check$pop, check$estimate
)

# scracht and extra checks ------------------------------------------------

"
check <- tidycensus::get_acs(
  table = 'B03002'
  ,year = yr
  ,state = state
  ,county = cofps
  ,geography = 'tract'
  ,cache_table = T
) %>%
  rename_with(tolower) %>%
  select(-name) %>%
  left_join(metadata[c('name', 'label')]
            , by = c('variable' = 'name'))
check %>% arrange(geoid)
check %>%
  mutate(var = censusrx::extract.acs.var(variable) ) %>%
  censusrx::acs.demographic.recode(
    filter.aggregates = F
  )# %>% View()"

