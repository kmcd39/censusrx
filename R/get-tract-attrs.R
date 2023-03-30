#' get.tract.attrs
#'
#' One massive function to pull a lot of frequently-used data points at the
#' tract level for a given year and set of counties. Will return a one-row-by
#' tract dataframe.
#'
#' Other acs pulls that are not 1-row by tract can be from `censusrx::tidycensus2recoded.tblList`
#'
#' @param state state fp code
#' @param cofps county fp codes (3-character)
#' @param year year.
#'
#' @export get.tract.attrs
get.tract.attrs <- function( state,
                             cofps,
                             year,
                             geo = 'tract') {

  require(tidyverse)
  require(sf)

  # browser()

  options(tigris_use_cache = TRUE)

  # geos -------------------------------------------------------------------------

  ctsf <- tigris::tracts(
    state = state
    ,county = cofps
    ,year = year
  ) %>%
    rename_with(tolower) %>%
    select(geoid, aland, geometry)

  # censusrx pulls

  # only get totals and medians; others aren't useful as 1-row/tract
  ctts <- censusrx::gett.census.totals(
    states = state
    ,cofps = cofps
    ,years = year
    ,geo = 'tract'
  )

  # skip this; they're useful for separate analysis.
  #acl <- censusrx::tidycensus2recoded.tblList(
  #  states = state
  #  ,years = year
  #  ,geo = 'tract'
  #)

  acm <- censusrx::pull.tidycensus.median.tables(
    states = state
    ,cofps = cofps
    ,years = year
    ,geo = 'tract'
  )

  ## reduce totals and medians

  # drop MOE and extra colms, rename estimate column with name:
  acm <- acm %>%
    imap( ~select(.x
                  ,geoid, year, !!.y := n)
    )

  attrs <-
    purrr::reduce(
      c(list( ctts
             ,tibble(ctsf)[c('geoid', 'aland')])
        , acm)
      ,full_join
    )

  # filter to counties (pulls used to be statewide, should be extraneous)
  attrs <- attrs %>%
    filter(substr(geoid, 3, 5) %in%
             cofps)

  # other pulls

  ## car ownership -----------------------------------------------------------

  # B08201_001	Total (universe: households)
  # B08201_002	Total: No vehicle available
  # B08201_003	Total: 1 vehicle available
  # B08201_004	Total: 2 vehicles available
  # B08201_005	Total: 3 vehicles available
  # B08201_006	Total: 4 or more vehicles available

  cown <- tidycensus::get_acs(
    geography = 'tract'
    ,variables = paste0('B08201_00', 1:6)
    ,year = year
    ,survey = 'acs5'
    ,state = state
    ,county = cofps
  ) %>%
    select(-NAME) %>%
    rename_with(tolower)

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

  # B08513_001	Total   -- universe is "Workers 16 Years and Over"
  # B08513_002	Total: Speak only English B08513_003	Total: Speak Spanish
  # B08513_004	Total: Speak Spanish: Speak English "very well" B08513_005	Total:
  # Speak Spanish: Speak English less than "very well" B08513_006	Total: Speak
  # other languages B08513_007	Total: Speak other languages: Speak English "very
  # well" B08513_008	Total: Speak other languages: Speak English less than "very
  # well"

  # skip this one both because the table is weird and it's not generally so
  # applicable.? Code in in indev folder if i wanna put back in


  ## LF and emply ------------------------------------------------------------

  lf <- tidycensus::get_acs(
    geography = 'tract'
    ,table = 'B23025'
    ,year = year
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

  # lf$geoid %>% duplicated() %>% sum()

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

  # B25003; universe is Occupied Housing Units
  tenure <- tidycensus::get_acs(
    geography = 'tract'
    ,table = 'B25003'
    ,year = year
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
    select(geoid, year,
           pop, n.hh,
           aland,
           n.hunits, occ.hunits,
           med.hhinc, med.crent, med.hcosts, med.hvalue,
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
    ))

  ## return
  attrs <- attrss %>%
    select(-aland)

  return(attrs)
}
