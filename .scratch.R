library(sf)
library(tidyverse)
library(mapview)
options(tigris_use_cache = TRUE)


# commute times by mode ---------------------------------------------------

geo <- 'county'

cofps <- '12011'

co <- tigris::counties(state = 12, year = 2020)

# acs table B08136 - AGGREGATE TRAVEL TIME TO WORK (IN MINUTES) OF WORKERS BY
# MEANS OF TRANSPORTATION TO WORK
cmts <- tidycensus::get_acs(
  geography = geo
  ,table = 'B08136'
  ,year = 2019
  #,county = cofps
  ,state = '12'
  ,cache_table = T
  ) %>%
  rename_with( tolower ) %>%
  select(-name)

cmts

cmt <- cmts %>%
  filter( geox::fix.geoid(geoid, 5) %in%
            cofps)
cmt

#' TABLE ENCODING:
#'
#' B08136_001	Aggregate travel time to work (in minutes)
#' B08136_002	Aggregate travel time to work (in minutes): Car, truck, or van
#' B08136_003	Aggregate travel time to work (in minutes): Car, truck, or van: Drove alone
#' B08136_004	Aggregate travel time to work (in minutes): Car, truck, or van: Carpooled
#' B08136_005	Aggregate travel time to work (in minutes): Car, truck, or van: Carpooled: In 2-person carpool
#' B08136_006	Aggregate travel time to work (in minutes): Car, truck, or van: Carpooled: In 3-or-more-person carpool
#' B08136_007	Aggregate travel time to work (in minutes): Public transportation (excluding taxicab)
#' B08136_008	Aggregate travel time to work (in minutes): Public transportation (excluding taxicab): Bus
#' B8136_009	Aggregate travel time to work (in minutes): Public transportation (excluding taxicab): Subway or elevated rail, Light rail, streetcar, or trolley (carro pÃƒÂºblico in Puerto Rico)
#' B08136_010	Aggregate travel time to work (in minutes): Public transportation (excluding taxicab): Long-distance train or commuter rail or Ferryboat
#' B08136_011	Aggregate travel time to work (in minutes): Walked
#' B08136_012	Aggregate travel time to work (in minutes): Taxicab, motorcycle, bicycle, or other means
#'
#' We want All car vs public trans breakout


# recode (note the tables are grouped differently; so will only do car and
# transit -- it's what's relevant anyway.)

cmt <- cmt %>%
  mutate( var =
            as.numeric(str_extract(variable, "[0-9]{3}$")))

# filter to just car and trans
cmt <- cmt %>%
  filter( var %in% c(2,7:10))

# recode
cmt <- cmt %>%
  mutate( recode =
            case_when(
              var %in% 2 ~ 'Car'
              ,var %in% 7 ~ 'Public transit (total)'
              ,var %in% 8 ~ 'Bus'
              ,var %in% c(9,10) ~ 'Train/subway'
              ,TRUE ~ as.character(NA)
            )) %>%
  group_by(geoid, recode) %>%
  summarise(agg.mins = sum(estimate))

cmt

# join total commuters - table B08006

#' B08006_001	Total
#' B08006_002	Total: Car, truck, or van
#' B08006_003	Total: Car, truck, or van: Drove alone
#' B08006_004	Total: Car, truck, or van: Carpooled
#' B08006_005	Total: Car, truck, or van: Carpooled: In 2-person carpool
#' B08006_006	Total: Car, truck, or van: Carpooled: In 3-person carpool
#' B08006_007	Total: Car, truck, or van: Carpooled: In 4-or-more-person carpool
#' B08006_008	Total: Public transportation (excluding taxicab)
#' B08006_009	Total: Public transportation (excluding taxicab): Bus
#' B08006_010	Total: Public transportation (excluding taxicab): Subway or elevated rail
#' B08006_011	Total: Public transportation (excluding taxicab): Long-distance train or commuter rail
#' B08006_012	Total: Public transportation (excluding taxicab): Light rail, streetcar or trolley (carro pÃƒÂºblico in Puerto Rico)
#' B08006_013	Total: Public transportation (excluding taxicab): Ferryboat
#' B08006_014	Total: Bicycle
#' B08006_015	Total: Walked
cmodes <-
  tidycensus::get_acs(
    geography = geo
    ,table = 'B08006'
    ,year = 2019
    #,county = cofps
    ,state = '12'
    ,cache_table = T
    ) %>%
  rename_with( tolower ) %>%
  select(-name)

cmode <- cmodes %>%
  mutate(geoid = geox::fix.geoid(geoid, 5)) %>%
  filter( geoid == cofps)

# add total
cmode <- cmode %>%
  group_by(geoid) %>%
  mutate(tot.commuters = .[variable == 'B08006_001',]$estimate)

# recode similarly
cmode <- cmode %>%
  group_by(geoid) %>%
  mutate( var =
            as.numeric(str_extract(variable, "[0-9]{3}$") )) %>%
  filter(var %in% c(2,8:12)) %>%
  mutate(recode =
           case_when(
             var %in% c(2) ~ 'Car'
             ,var %in% 8 ~ 'Public transit (total)'
             ,var %in% 9 ~ 'Bus'
             ,var %in% c(10:12) ~ 'Train/subway'
             ,TRUE ~ 'Other') # label)
  ) %>%
  group_by(geoid, recode) %>%
  summarise(n.commuters = sum(estimate)) %>%
  mutate(mode.perc =
           n.commuters /
           sum(n.commuters)) %>%
  ungroup()

cmt <- cmode %>%
  left_join(cmt) %>%
  mutate(avg.mins = agg.mins / n.commuters)

cmt


# check fcnalization ------------------------------------------------------

devtools::document()
devtools::load_all()
ca <- commute.times.by.mode('county', 'CA', 2019)

ca
fl <- commute.times.by.mode('county', 'FL', 2019)
fl %>% filter(geoid %in% cofps)
# check for LA county..


# add in-city identifiers; get total for city and MSA
cmode <- cmode %>%
  left_join(ct2cpa) %>%
  mutate(in.LA.city = !is.na(cpa.name)) %>%
  group_by(recode) %>%  # MSA counts
  mutate(n.msa = sum(n)) %>%
  group_by(in.LA.city, recode) %>%
  mutate(n.city = sum(n)) %>%  # City counts
  ungroup() %>%
  filter(in.LA.city) %>%   # end w just city/msa counts
  select(recode, n.msa, n.city) %>%
  distinct()

# pivot long by region type.
cmode <- cmode %>%
  pivot_longer( where(is.numeric) ) %>%
  mutate(rt =
           str_extract(name, 'city|msa')) %>%
  mutate(var = str_extract(name, 'n|tot|perc')) %>%
  select(-name) %>%
  pivot_wider(names_from = var
              ,values_from = value)  %>%
  select(rt, recode, n) %>%
  arrange(rt)


# add percents & totals
cmode <- cmode %>%
  group_by(rt) %>%
  mutate( tot = sum(n)
          ,perc = 100 * n / sum(n)) %>%
  ungroup()

# (join, finally)
mst <- mst %>%
  inner_join(cmode) %>%
  mutate(avg.commute.time =
           agg.mins / n)
