

#' tidycensus2recoded.tblList
#'
#' Wrapper function to pull all categorical information for which there's recode
#' functions written. Right now this includes:
#' - B03002 (demographics; universe is Total Pop)
#' - B25034 (building age; universe is Housing Units)
#' - B25004 (vacant unit types; universe is Vacant Housing Units)
#' - B25070 (Rent burden counts; universe is Renter-occupied Housing Units)
#' - B08006 (Commute mode; universe is Workers 16 Years and Over)
#'
#' @param states vector of state FIPS or 2-letters
#' @param years vector of years
#' @param geo selection geography, passed onto `tidycensus`
#'
#' @export tidycensus2recoded.tblList
tidycensus2recoded.tblList <- function(states, years
                                         ,geo = 'tract'
                                         ,tbls =
                                           c('B03002',
                                             'B25034',
                                             'B25004',
                                             'B25070',
                                             'B08006')
                                       , metadata = NULL
                                       ,survey = 'acs5'
                                       ) {
  require(tidyverse)
  require(tidycensus)

  if( is.null(metadata))
    metadata <- pull.acs.metadata(year = tail(years, 1))

  x <- map(tbls,
           ~multiyr.acs.wrapper(
             tables = .x
             ,geo = geo
             ,years = years
             ,states = states
             ,metadata = metadata
             ,survey = survey
           ) ) %>%
    setNames(tbls)

  fcns <- c(acs.demographic.recode,
            acs.bldg.age.recode,
            acs.vacancy.recode,
            acs.rentburden.recode,
            acs.commute.recode
  )

  # call appropriate recode fcn for each table
  rx <- map2(x, fcns
             , ~.y(.x)
  )

  # sum to recode, and add %s
  rx <- map(rx,
            ~{.x %>%
                group_by(yr, geoid, recode) %>%
                summarise(n = sum(estimate)) %>%
                group_by(yr, geoid) %>%
                mutate(perc = n / sum(n)) %>%
                ungroup()
            })

  return(rx)
}


#' acs.tbl.index
#'
#' Reminds you what selected table names correspond to
#'
#' @export acs.tbl.index
acs.tbl.index <- function(
  tables =  c('B03002',
              'B25034',
              'B25004',
              'B25070',
              'B19013',
              'B25058',
              'B08006')
  ,metadata = NULL
  ,year = 2019) {

  require(tidyverse)

  if( is.null(metadata))
    metadata <- pull.acs.metadata(year = year)

  metadata <- metadata %>%
    mutate( table =
              str_extract( name, '[^_]+')
            ,concept = tolower(concept)
            ) %>%
    filter(table %in% tables ) %>%
    select(table, concept) %>%
    distinct()

  return(metadata)
}

#' pull.tidycensus.median.tables
#'
#' Pulls median tables, like hh income and contract rent:
#' - B19013 (median household income; universe is households)
#' - B25058 (median contract rent; universe is "Renter-occupied Housing Units Paying Cash Rent")
#' - B25077 (median home value; universe is owner-occupied housing units)
#'
#' @inheritParams tidycensus2recoded.tblList
#'
#' @export pull.tidycensus.median.tables
pull.tidycensus.median.tables <- function(states, years
                               ,geo = 'tract'
                               ,tbls =
                                 c('B19013' # hh inc
                                   ,'B25058' # c rent
                                   ,'B25077' # median home value
                                 )
                               ,survey = 'acs5'
) {
  require(tidyverse)

  mx <- map(tbls,
            ~multiyr.acs.wrapper(
              tables = .x
              ,geo = geo
              ,years = years
              ,states = states
              ,survey = survey
            ) ) %>%
    setNames(tbls)


  # rename estimate to N to match other tbls
  mx <- map(mx, ~rename(.x, n = estimate))

  return(mx)
}





#' gett.census.totals
#'
#' Gets totals for area. Like population, households, housing units, and land
#' area. Wraps `tidycensus::get_acs`. 1 year at a time please.
#'
#' @inheritParams tidycensus2recoded.tblList
#'
#' @export gett.census.totals
gett.census.totals <- function(states, years
                       ,geo = 'tract'
                       ,survey = 'acs5') {

  require(tidyverse)

  params <- expand.grid(states, years)

  tots <- map2_dfr( params[[1]], params[[2]]
                    ,~{ tidycensus::get_acs(
                      geography = geo
                      ,variables =
                        c( pop = 'B01001_001'
                           ,n.hh = 'B08201_001' #n.households
                           ,n.hunits = 'B25034_001' # n housing units
                        )
                      ,state = .x
                      #,county = substr(., 3,5)
                      ,year = .y
                      ,survey = survey
                      ,geometry = F
                      ,cache_table = T
                    ) %>%
                        mutate(yr = .y
                        ) %>%
                        rename_with( tolower )
                    }) %>%
    select(-name)

  # pivot wide
  tots <- tots %>%
    select(-moe) %>%
    pivot_wider(names_from = variable
                ,values_from = estimate)

  return(tots)
}




#' get.all.rentals.by.price.lvl
#'
#' ACS by default includes "vacant" and "occupied" housing units separately in
#' many tables. This function wraps the recode functions for rentals by price
#' tables:
#'
#' * B25056 (universe: renter-occupied housing units)
#'
#' * B25061 (universe: Vacant-for-rent and Rented, Not Occupied Housing Units)
#'
#' @export get.all.rentals.by.price.lvl
get.all.rentals.by.price.lvl <- function( states
                                          ,years
                                          ,geo
                                          ,survey = 'acs5') {

  require(tidyverse)

  params <- expand.grid(states, years)

  occ.rent <- map2_dfr( params[[1]], params[[2]]
                    ,~{ tidycensus::get_acs(
                      geography = geo
                      ,table = 'B25056'
                      ,state = .x
                      #,county = substr(., 3,5)
                      ,year = .y
                      ,survey = survey
                      ,geometry = F
                      ,cache_table = T
                    ) %>%
                        mutate(yr = .y
                        ) %>%
                        rename_with( tolower )
                    }) %>%
    select(-name) %>%
    mutate(var = extract.acs.var(variable))

  vac.rent <- map2_dfr( params[[1]], params[[2]]
                        ,~{ tidycensus::get_acs(
                          geography = geo
                          ,table = 'B25061'
                          ,state = .x
                          #,county = substr(., 3,5)
                          ,year = .y
                          ,survey = survey
                          ,geometry = F
                          ,cache_table = T
                        ) %>%
                            mutate(yr = .y
                            ) %>%
                            rename_with( tolower )
                        }) %>%
    select(-name) %>%
    mutate(var = extract.acs.var(variable))

  occ.rent <- occ.rent %>%
    acs.rentals.by.level.recode(
      acs.tbl = 'B25056'
    )
  vac.rent <- vac.rent %>%
    acs.rentals.by.level.recode(
      acs.tbl = 'B25061'
    )

  rentals <- rbind(occ.rent
                   ,vac.rent)

  rentals <- rentals %>%
    filter(!is.na(recode)) %>%
    group_by(yr, geoid, recode) %>%
    summarise(n = sum(estimate))

  return(rentals)
}

# metawrapper -------------------------------------------------------------

#' mass.acs.pull.tract.attrs
#'
#' Wrapps all the other ACS/tidycensus wrapper functions here to pull a
#' selection of tables with recoded values, medians, and totals, over a
#' selection of states and years.
#'
#' You can use `taux::acs.tbl.index` for reminders of what each table is.
#'
#' @inheritParams tidycensus2recoded.tblList
#'
#' @export mass.acs.pull.wrapper
mass.acs.pull.wrapper <- function(state,
                                  county,
                                  year) {


  ts <- tidycensus2recoded.tblList( states
                                   ,years
                                   ,geo = geo)


  ms <- pull.tidycensus.median.tables( states
                                      ,years
                                      ,geo = geo)

  tots <- gett.census.totals(states
                             ,years
                             ,geo = geo)

  tenure <- multiyr.acs.wrapper( tables = 'B25003'
                                ,states = states
                                ,years = years
                                ,geo = geo)

  out <- c(ts,
           ms,
           list(tenure = tenure),
           list(totals = tots))

  return(out)

}
