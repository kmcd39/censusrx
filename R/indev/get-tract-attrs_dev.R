#' get.tract.attrs
#'
#' One massive function to pull a lot of frequently-used data points at the
#' tract level for a given year and set of counties. Will return a one-row-by tract dataframe.

# use browser call w/in function to hash out expanding fcn...

library(tidyverse)
library(sf)
library(mapview)

rm(list = ls())
options(tigris_use_cache = TRUE)

devtools::load_all()

# params ------------------------------------------------------------------

state <- 37
cofps <- c('053'
           ,'055'
           )
yr <- 2021

# pull metadata
metadata <- pull.acs.metadata(year = yr)

# can i make tract.attrs.more flexible ------------------------------------

# i.e., adding counties

get.tract.attrs_dev <- function(
    state,
    cofps,
    year,
    geo = 'tract'
    #,geo.fcn = NULL
    ,get.demographics.and.commute = F
) {

  require(tidyverse)
  require(sf)

  #browser()

  options(tigris_use_cache = TRUE)

  # get metadata (slight efficiency)
  # geos -------------------------------------------------------------------------

  if(geo == 'tract') {
    nbhds <- tigris::tracts(
      state = state
      ,county = cofps
      ,year = year)
  } else if(geo == 'block group') {
    nbhds <- tigris::block_groups(
      state = state
      ,county = cofps
      ,year = year)
  } else if(geo == 'county') {
    nbhds <- tigris::counties(
      state = state
      ,year = year)
  }

  nbhds <- nbhds %>%
    tibble() %>%
    rename_with(tolower) %>%
    select(geoid, aland)#, geometry)

  # censusrx pulls

  # only totals and medians
  ctts <- censusrx::gett.census.totals(
    states = state
    ,cofps = cofps
    ,years = year
    ,geo = geo
  )


  acm <- censusrx::pull.tidycensus.median.tables(
    states = state
    ,cofps = cofps
    ,years = year
    ,geo = geo
  )

  ## reduce totals and medians
  # drop MOE and extra colms, rename estimate column with name:
  acm <- acm %>%
    imap( ~select(.x
                  ,geoid, year, !!.y := n)
    )


  # get demos and commuting -------------------------------------------------

  if(get.demographics.and.commute) {
    # these tables aren't 1-row/tract, but useful for pulling % car commuters, %
    # bl, etc.
    acl <- censusrx::tidycensus2recoded.tblList(
      states = state
      ,cofps = cofps
      ,years = year
      ,geo = geo
    )

    # B03002 (demographics; universe is Total Pop); B08006 (Commute mode; universe
    # is Workers 16 Years and Over)
    acl <- acl[c("B03002", "B08006")]
    acl$B03002 <-
      acl$B03002 %>%
      mutate(recode =
               str_extract(tolower(recode)
                           ,"black|latino|asian|other")
      ) %>%
      filter(!is.na(recode)) %>% # drop white only (remainder after subtracting others from pop)
      mutate(recode = if_else(recode == "other", "other.nonwhite", recode)
      ) %>%
      pivot_wider(
        names_from = recode
        ,values_from = c(n, perc)
      )

    acl$B08006 <-
      acl$B08006 %>%
      filter(grepl("transit", recode)) %>%
      group_by(year, geoid) %>%
      summarise( across(c(n, perc),
                        list(transit = sum))
                 ) %>%
      ungroup()
  }



  ## car ownership -----------------------------------------------------------

  # B08201_001	Total (universe: households)
  # B08201_002	Total: No vehicle available
  # B08201_003	Total: 1 vehicle available
  # B08201_004	Total: 2 vehicles available
  # B08201_005	Total: 3 vehicles available
  # B08201_006	Total: 4 or more vehicles available
  cown <- tidycensus::get_acs(
    geography = geo
    ,variables = paste0('B08201_00', 1:2# 1:6
                        )
    ,year = year
    ,survey = 'acs5'
    ,state = state
    ,county = cofps
  ) %>%
    select(-NAME) %>%
    rename_with(tolower)

  # pivot wide, get perc, and drop n.hh, which will be duplicative with n.hh
  # from ctts.
  cownt <- cown %>%
    select(-moe) %>%
    #filter(grepl('1$|2$', variable)) %>%
    pivot_wider(names_from = variable,
                values_from = estimate) %>%
    mutate(perc.no.car =
             B08201_002 / B08201_001) %>%
    select(-B08201_001) %>%
    rename(nhh.zerocar = B08201_002)

  ## language ----------------------------------------------------------------

  # B08513_001	Total   -- universe is "Workers 16 Years and Over"
  # B08513_002	Total: Speak only English B08513_003	Total: Speak Spanish
  # B08513_004	Total: Speak Spanish: Speak English "very well" B08513_005	Total:
  # Speak Spanish: Speak English less than "very well" B08513_006	Total: Speak
  # other languages B08513_007	Total: Speak other languages: Speak English "very
  # well" B08513_008	Total: Speak other languages: Speak English less than "very
  # well"

  # skip this one for now because the recode is annoying? Code in in indev
  # folder if i wanna put back in

  ## LF and emply ------------------------------------------------------------

  lbls <- tibble(
    variable = Hmisc::Cs(
      B23025_001
      ,B23025_002
      #,B23025_003
      #,B23025_004
      ,B23025_005
      #,B23025_006
      #,B23025_007
    )
    ,lbl = c(
      'pop.over.16'
      ,'inLF'
      #,'in.civLF'
      #,'employed'
      ,'n.unemployed'
      #,'armed.forces'
      #,'not.inLF'
    )
  )

  lf <- tidycensus::get_acs(
    geography = geo
    #,table = 'B23025'
    ,variables = set_names(lbls$variable, lbls$lbl)
    ,year = year
    ,survey = 'acs5'
    ,state = state
    ,county = cofps
  ) %>%
    select(-NAME) %>%
    rename_with(tolower)

  lf <- lf %>%
    select( -moe ) %>%
    pivot_wider(values_from = estimate
                ,names_from = variable) %>%
    mutate( lfpr =
              inLF / pop.over.16
            ,unemply.rate =
              n.unemployed /  inLF
    ) %>%
    select( -pop.over.16 )

  ## tenure ------------------------------------------------------------------

  # B25003; universe is Occupied Housing Units
  tenure <- tidycensus::get_acs(
    geography = geo
    #,table = 'B25003'
    ,variables = c( 'occ.hunits' = 'B25003_001'
                   ,'rental.occ.hu' = 'B25003_003'
                    )
    ,year = year
    ,survey = 'acs5'
    ,state = state
    ,county = cofps
    #,output = 'wide'
  ) %>%
    select(-NAME) %>%
    rename_with(tolower)

  tenure <- tenure %>%
    select( -moe ) %>%
    pivot_wider(names_from = variable,
                values_from = estimate) %>%
    mutate(rental.rate =
             rental.occ.hu / occ.hunits)

  # final transforms ------------------------------------------------------------------

  ## reduce ------------------------------------------------------------------

  attrs <-
    purrr::reduce(
      c(
        list(ctts, tibble(nbhds)[c('geoid', 'aland')])
        , acm
        ,list(cownt)
        ,list(lf)
        ,list(tenure)
        )
      ,full_join
    )

  if(get.demographics.and.commute)
    attrs <- attrs %>%
    full_join(acl$B03002) %>%
    full_join(acl$B08006)

  ## reorder -----------------------------------------------------------------

  colnames(attrs)

  attrss <- attrs %>%
    select(geoid, year,
           pop, n.hh,
           aland,
           n.hunits, occ.hunits,
           med.hhinc, med.crent, med.hcosts, med.hvalue,
           inLF, lfpr, n.unemployed, unemply.rate,
           nhh.zerocar, perc.no.car,
           rental.occ.hu, rental.rate,
           any_of(c("n_transit", "perc_transit",
                    "n_other.nonwhite", "n_asian", "n_latino", "n_black", "perc_other.nonwhite")
                  )
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
    ))

  attrs <- attrss %>%
    select(-aland)

  ## return
  return(attrs)
}


coattrs <-
  get.tract.attrs_dev(
  state = "25",
  cofps = NULL,
  year = 2021,
  geo = 'county'
  #,geo.fcn = NULL
  ,get.demographics.and.commute = T
)

coattrs %>% glimpse()
coattrs
# previous ----------------------------------------------------------------



get.tract.attrs(state, cofps, yr, geo = "tract")

tmp.w.bonus <-
  get.tract.attrs(state, cofps, yr, geo = "tract"
                  ,get.demographics.and.commute = T)

tmp.w.bonus %>%
  geox::attach.geos(year = yr) %>%
  mapview::mapview(zcol = "perc_transit")
  #mapview::mapview(zcol = "med.hcosts")



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

# these tables aren't 1-row/tract, but useful for pulling % car commuters, %
# bl, etc.
acl <- censusrx::tidycensus2recoded.tblList(
  states = state
  ,cofps = cofps
  ,years = yr
  ,geo = 'tract'
)

acl

acm <- censusrx::pull.tidycensus.median.tables(
  states = state
  ,years = yr
  ,cofps = cofps
  ,geo = 'tract'
)

# drop MOE and extra colms, rename estimate column with name:
names(acm)
acm <- acm %>%
  imap( ~select(.x
                ,geoid, year, !!.y := n)
  )

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


