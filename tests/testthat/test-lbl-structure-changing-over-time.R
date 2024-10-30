library(dplyr)
library(tidyr)
library(purrr)
library(sf)
library(mapview)


meta21 <- censusrx::pull.acs.metadata(
  year = 2021
  ,dataset = "acs5"
)

meta13 <- censusrx::pull.acs.metadata(
  year = 2013
  ,dataset = "acs5"
)

acs.commute.recode
meta21 %>%
  select(name, lbl21 = label) %>%
  filter(grepl("B08006", name)
         ) %>%
  left_join(meta13 %>%
              filter(grepl("B08006", name)) %>%
              select(name, lbl13 = label)) %>%
  filter(lbl21 != lbl13
         ) %>% View()

commute22 <- censusrx::commute.times.by.mode(
  geo = "state"
  ,state = 25
  #,cofps = "015" # hampshire county, MA
  ,year = 2022
)


commute22

commute13 <- censusrx::commute.times.by.mode(
  geo = "state"
  ,state = 25
  #,cofps = "015" # hampshire county, MA
  ,year = 2013
)

commute13
commute22

