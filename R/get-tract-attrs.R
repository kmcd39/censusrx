#' get.tract.attrs
#'
#' One massive function to pull a lot of frequently-used data points at the
#' tract or block group level for a given year and set of counties. Will return
#' a one-row-by tract (or BG) dataframe.
#'
#' TODO: could be add an argument for keeping MOEs
#'
#' Other acs pulls that are not 1-row by tract can be from
#' `censusrx::tidycensus2recoded.tblList`
#'
#' @param state state fp code
#' @param cofps county fp codes (3-character)
#' @param year year.
#' @param geo "tract" (default) or "block group" or "county"
#' @param get.demographics.and.commute Whether to add some demographic and
#'   commute information. These may be pulled separately with
#'   `censusrx::tidycensus2recoded.tblList` -- b/c they don't have an obvious 1
#'   row/tract, this would allow more specific analysis. When they are pulled
#'   here they are pulled in wide, where the "transit" columns will refer to
#'   combined active+public transit.
#'
#' @export get.tract.attrs
get.tract.attrs <- function( state,
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
