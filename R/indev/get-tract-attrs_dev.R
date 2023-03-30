#' get.tract.attrs
#'
#' One massive function to pull a lot of frequently-used data points at the
#' tract level for a given year and set of counties. Will return a one-row-by tract dataframe.

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

ctsf

# censusrx pulls ----------------------------------------------------------

#' only get totals and medians; others aren't useful as 1-row/tract

ctts <- censusrx::gett.census.totals(
  states = state
  ,years = yr
  ,cofps = cofps
  ,geo = 'tract'
)

ctts

# skip this; they're useful for separate analysis.
#acl <- censusrx::tidycensus2recoded.tblList(
#  states = state
#  ,years = yr
#  ,geo = 'tract'
#)

acm <- censusrx::pull.tidycensus.median.tables(
  states = state
  ,years = yr
  ,cofps = cofps
  ,geo = 'tract'
)

#names(acl)
names(acm)

## reduce totals and medians -----------------------------------------------

# drop MOE and extra colms, useful names:
acm <- acm %>%
  map2( c('medhhinc', 'medcrent', 'medhomevalue')
        ,~select(.x
                 ,geoid, yr, !!.y := n)
        )

attrs <-
  purrr::reduce(
     c(list(ctts, tibble(ctsf)[c('geoid', 'aland')])
       , acm)
    ,full_join
  )

# filter to counties (b/c those pulls are statewide)
attrs <- attrs %>%
  filter(substr(geoid, 3, 5) %in%
           cofps)

attrs

# other pulls -------------------------------------------------------------


## car ownership -----------------------------------------------------------

#' B08201_001	Total (universe: households)
#' B08201_002	Total: No vehicle available
#' B08201_003	Total: 1 vehicle available
#' B08201_004	Total: 2 vehicles available
#' B08201_005	Total: 3 vehicles available
#' B08201_006	Total: 4 or more vehicles available

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

cown
cownt <- cown %>%
  select(-moe) %>%
  filter(grepl('1$|2$', variable)) %>%
  pivot_wider(names_from = variable,
              values_from = estimate) %>%
  mutate(perc.no.car =
           B08201_002 / B08201_001)

# B08201_001, n.hh will be duplicative with n.hh from ctts.
cownt <- cownt %>%
  rename(n.hh = B08201_001, nhh.zerocar = B08201_002 ) %>%
  select(-n.hh)

attrs <- attrs %>%
  left_join(cownt)

## language ----------------------------------------------------------------

#' B08513_001	Total   -- universe is "Workers 16 Years and Over"
#' B08513_002	Total: Speak only English B08513_003	Total: Speak Spanish
#' B08513_004	Total: Speak Spanish: Speak English "very well" B08513_005	Total:
#' Speak Spanish: Speak English less than "very well" B08513_006	Total: Speak
#' other languages B08513_007	Total: Speak other languages: Speak English "very
#' well" B08513_008	Total: Speak other languages: Speak English less than "very
#' well"
#'
#'

# skip this one both because the table is weird and it's not generally so
# applicable.?

"
lang <- tidycensus::get_acs(
  geography = 'tract'
  ,variables = paste0('B08201_00',
                      1:8)
                      #c(1, 2, 4, 7))
  ,year = yr
  ,survey = 'acs5'
  ,state = state
  ,county = cofps
) %>%
  select(-NAME) %>%
  rename_with(tolower)

lang <- lang %>%
  mutate(var = extract.acs.var(variable))
# check how columns sum? overlap btwn 004/007?
lang %>%
  #filter(var != 1) %>%
  #filter( ! var %in% c(1, 3, 6)) %>%
  filter(var %in% c(2,4,5,7, 8)) %>%
    group_by(geoid) %>%
  summarise(n = sum(estimate)) %>%
  arrange(geoid)

lang %>%
  filter(var == 1) %>%
  arrange(geoid)

lang <- lang %>%
  mutate(recode =
           if_else( grepl('2$|4$|7$', variable)
                    ,'speaks.english'
                    ,'tot.workers')) %>%
  group_by(geoid, recode) %>%
  summarise(n = sum(estimate))

# widen and add to attrs
attrs <- lang %>%
  ungroup() %>%
  pivot_wider(
    names_from = recode,
    values_from = n
  ) %>%
  inner_join(attrs)

# note i think issue with MOEs pushing the % > 100 for 2 tracts; correct here
attrs  <- attrs %>%
  mutate(perc.engl.speakers =
           speaks.english/ tot.workers) %>%
  mutate(perc.engl.speakers =
           if_else(perc.engl.speakers > 1,
                   1, perc.engl.speakers))

attrs"

## LF and emply ------------------------------------------------------------

lf <- tidycensus::get_acs(
  geography = 'tract'
  ,table = 'B23025'
  ,year = yr
  ,survey = 'acs5'
  ,state = state
  ,county = cofps
) %>%
  select(-NAME) %>%
  rename_with(tolower)

lbls <- tibble(
  variable = Hmisc::Cs(
    B23025_001
    ,B23025_002
    ,B23025_003
    ,B23025_004
    ,B23025_005
    ,B23025_006
    ,B23025_007)
  ,lbl = c(
    'pop.over.16'
    ,'inLF'
    ,'in.civLF'
    ,'employed'
    ,'unemployed'
    ,'armed.forces'
    ,'not.inLF'
  )
)

lf <- lf %>%
  left_join(lbls) %>%
  select(geoid, lbl, estimate) %>%
  pivot_wider(values_from = estimate
              ,names_from = lbl)

lf$geoid %>% duplicated() %>% sum()

lf <- lf %>%
  mutate( lfpr =
            inLF / pop.over.16

          ,unemply.rate =
            unemployed /  inLF
  ) %>%
  select(geoid, inLF, lfpr, n.unemployed = unemployed, unemply.rate)

attrs <- attrs %>%
  left_join(lf)

## tenure ------------------------------------------------------------------

#' B25003; universe is Occupied Housing Units
tenure <- tidycensus::get_acs(
  geography = 'tract'
  ,table = 'B25003'
  ,year = yr
  ,survey = 'acs5'
  ,state = state
  ,county = cofps
  ,output = 'wide'
) %>%
  select(-NAME) %>%
  rename_with(tolower)

tenure <- tenure %>%
  select(-matches('m$')) %>%
  select( geoid
         ,occ.hunits = b25003_001e
         ,rental.occ.hu = b25003_003e) %>%
  mutate(rental.rate = rental.occ.hu / occ.hunits)

attrs <- attrs %>%
  left_join(tenure)

# final transforms ------------------------------------------------------------------

## reorder -----------------------------------------------------------------

colnames(attrs)

attrss <- attrs %>%
  select(geoid, yr,
         pop, n.hh,
         aland,
         n.hunits, occ.hunits,
         medhhinc, medcrent, medhomevalue,
         inLF, lfpr, n.unemployed, unemply.rate,
         nhh.zerocar, perc.no.car,
         rental.occ.hu, rental.rate
  )

sum(! colnames(attrs) %in% colnames(attrss))

## put into acres and add densities ----------------------------------------

attrss <- attrss %>%
  mutate(aland.acre =
           units::set_units(
             units::set_units(aland, 'm^2')
             ,'acres'
           )) %>%
  mutate(across(c(pop, n.hunits)
                ,list(dens = ~.x / aland.acre)
  )) #%>%

attrs
#select(matches('aland|dens|pop|n.hu'))


## return -------------------------------------------------------------------

attrs <- attrss %>%
  select(-aland)

attrs


