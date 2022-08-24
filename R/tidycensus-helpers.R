

# little helpers ----------------------------------------------------------

#' extract.acs.var
#'
#' From acs table with Btable#_var# format, extract the variable. Takes the
#' column called "variable" for table pulled with tidycensus.
#'
#' @export extract.acs.var
extract.acs.var <- function(x) {

      as.numeric(
        str_extract(x, '[0-9]{3}$')
    )

}

# pulling data wrappers -----------------------------------------------------



#' multiyr.acs.wrapper
#'
#' Wraps `tidycensus::get_acs` for multiple years, tables, and states.
#' Structures long by geography, table, year. Maps through tables/years.
#' Tidycensus vectorized over states.
#'
#' I'll need to figure out and maybe refine this function to work with
#' reasonable combinations of states/geographies.
#'
#' @param table table to get from ACS; i.e., 'B01001.' See
#'   `tidycensus::load_variables`
#' @param states,geo,years passed to `tidycensus::get_acs`. There is one get_acs
#'   call for every combination of table and years.
#' @param metadata To use for labels. Result of `pull.acs.metadata` or
#'   `tidycensus::load_variables`
#'
#' @export multiyr.acs.wrapper
multiyr.acs.wrapper <- function( tables
                                 , states
                                 , geo
                                 , years = c(2019)
                                 , metadata = NULL
                                 ,cache = T
                                 ,survey = 'acs5') {


  if( is.null(metadata))
    metadata <- pull.acs.metadata(year = tail(years, 1))

  #browser()
  params <- expand.grid(tables, years)

  x <- map2_dfr( params[[1]], params[[2]]
                 , ~{tidycensus::get_acs(
                   geography = geo
                   ,table = .x
                   ,year = .y
                   ,state = states
                   ,survey = survey
                   ,cache_table = cache
                 ) %>%
                     mutate(tabl = .x
                            ,yr = .y
                     ) %>%
                     rename_with( tolower )
                 })

  x <- x %>% select(-name)

  # add labels
  x <- x %>%
    left_join(metadata[c('name', 'label')]
              , by = c('variable' = 'name')) %>%
    mutate(var =
             extract.acs.var(variable)
           ,.after = tabl
             )
  return(x)
}


#' pull.acs.metadata
#'
#' Pulls ACS metadata and cleans labels.
#'
#'@export pull.acs.metadata
pull.acs.metadata <- function(year
                              ,dataset = 'acs5'
                              ,cache = T) {

  require(tidyverse)

  meta <- tidycensus::load_variables(year = year
                             ,dataset = dataset
                             ,cache = cache)

  # clean
  meta <- meta %>%
    mutate(label = gsub('!!', ' ', label)) %>%
    mutate(label = gsub('Estimate ', '', label)) %>%
    mutate(label = gsub(':$', '', label))

  return(meta)
}


# recoding fcns -----------------------------------------------------------

#' acs.demographic.recode
#'
#' Recodes demographic info, as from table B03002. Was developed for 2019 ACS;
#' could break if they change encodings.
#'
#' Universe is Total Population.
#'
#' Encodings this was developed for:
#' B03002_001	Total
#' B03002_002	Total: Not Hispanic or Latino
#' B03002_003	Total: Not Hispanic or Latino: White alone
#' B03002_004	Total: Not Hispanic or Latino: Black or African American alone
#' B03002_005	Total: Not Hispanic or Latino: American Indian and Alaska Native alone
#' B03002_006	Total: Not Hispanic or Latino: Asian alone
#' B03002_007	Total: Not Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone
#' B03002_008	Total: Not Hispanic or Latino: Some other race alone
#' B03002_009	Total: Not Hispanic or Latino: Two or more races
#' B03002_010	Total: Not Hispanic or Latino: Two or more races: Two races including Some other race
#' B03002_011	Total: Not Hispanic or Latino: Two or more races: Two races excluding Some other race, and three or more races
#' B03002_012	Total: Hispanic or Latino
#' B03002_013	Total: Hispanic or Latino: White alone
#' B03002_014	Total: Hispanic or Latino: Black or African American alone
#' B03002_015	Total: Hispanic or Latino: American Indian and Alaska Native alone
#' B03002_016	Total: Hispanic or Latino: Asian alone
#' B03002_017	Total: Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone
#' B03002_018	Total: Hispanic or Latino: Some other race alone
#' B03002_019	Total: Hispanic or Latino: Two or more races
#' B03002_020	Total: Hispanic or Latino: Two or more races: Two races including Some other race
#' B03002_021	Total: Hispanic or Latino: Two or more races: Two races excluding Some other race, and three or more races
#'
#'
#' @param demos a demographics table, as from `multiyr.acs.wrapper(B03002, ...)`.
#' @param other.vars Vars (number following underscore) for groups to put into an "Other group" category.
#' @param filter.aggregates If true, filter totals and subtotals.
#'
#' @export acs.demographic.recode
acs.demographic.recode <- function(demos
                                   ,other.vars = c(5, 7:11)
                                   ,filter.aggregates = T) {

  if(filter.aggregates)
    demos <- demos %>%
      filter( ! var %in% c(1, 2, 13:21) )

  demos <- demos %>%
    mutate( label = gsub('Total: Not Hispanic or Latino: ', '', label)) %>%
    mutate( label = gsub('Total: ', '', label)) %>%

    mutate(recode = case_when(
      var %in% other.vars ~ 'Other group'
      ,TRUE ~ label)
    )

  # reorder groups in reverse (so "Other" is last)
  demos$recode <- factor(demos$recode
                         ,levels =
                           rev(
                           c('White alone'
                           ,'Black or African American alone'
                           ,'Hispanic or Latino'
                           ,'Asian alone'
                           ,'American Indian and Alaska Native alone'
                           ,'Native Hawaiian and Other Pacific Islander alone'
                           ,'Other group'
                         )))

  return(demos)
}



#' acs.vacancy.recode
#'
#' Recodes demographic info, as from table B25004 Was developed for 2019 ACS;
#' could break if they change encodings.
#'
#' Universe for this table is Vacant Housing Units.
#'
#' B25004_001	Total
#' B25004_002	Total: For rent
#' B25004_003	Total: Rented, not occupied
#' B25004_004	Total: For sale only
#' B25004_005	Total: Sold, not occupied
#' B25004_006	Total: For seasonal, recreational, or occasional use
#' B25004_007	Total: For migrant workers
#'
#' @param vacancy a vacancy table, as from `multiyr.acs.wrapper(B25004, ...)`.
#' @inheritParams acs.demographic.recode
#'
#' @export acs.vacancy.recode
acs.vacancy.recode <- function(vacancy
                               ,filter.aggregates = T
                               ) {

  if(filter.aggregates)
    vacancy <- vacancy %>%
      filter( ! var %in% c(1) )

  vacancy <- vacancy %>%
    mutate( label = gsub('Total: ', '', label)) %>%
    mutate(recode = case_when(
      var %in% 1:5 ~ 'For rent/sale, or just rented/sold'
      ,TRUE ~ label)
    )

  # also use factor vacancy status
  vacancy$recode <- factor(vacancy$recode
                           , levels = c(
                              'For migrant workers'
                             ,'For seasonal, recreational, or occasional use'
                             ,'Other vacant'
                             ,'For rent/sale, or just rented/sold'
                           ))

  return(vacancy)
}




#' acs.bldg.age.recode
#'
#' Recodes demographic info, as from table B25034 Was developed for 2019 ACS;
#' could break if they change encodings. Note also table B25035_001 is just
#' median year structure built.
#'
#' Universe for this table is Housing Units.
#'
#' B25034_001	Total
#' B25034_002	Total: Built 2014 or later
#' B25034_003	Total: Built 2010 to 2013
#' B25034_004	Total: Built 2000 to 2009
#' B25034_005	Total: Built 1990 to 1999
#' B25034_006	Total: Built 1980 to 1989
#' B25034_007	Total: Built 1970 to 1979
#' B25034_008	Total: Built 1960 to 1969
#' B25034_009	Total: Built 1950 to 1959
#' B25034_010	Total: Built 1940 to 1949
#' B25034_011	Total: Built 1939 or earlier
#'
#'
#' @param vacancy a building-age table, as from `multiyr.acs.wrapper(B25034, ...)`.
#' @inheritParams acs.demographic.recode
#'
#'
#' @export acs.bldg.age.recode
acs.bldg.age.recode <- function(bldgs
                                ,filter.aggregates = T) {

  if(filter.aggregates)
    bldgs <- bldgs %>%
      filter( ! var %in% c(1) )

  bldgs <- bldgs %>%
    mutate( label = gsub('Total: ', '', label)) %>%
    mutate(recode = case_when(
      var %in% c(2:4) ~ 'Built since 2000'
      ,var %in% c(5:7) ~ 'Built from 1970-1999'
      ,var %in% c(8:11) ~ 'Built before 1970'
      ,TRUE ~ label
    ))

  # as factor
  bldgs$recode <- factor(bldgs$recode
                         ,levels = rev(c('Built before 1970'
                                         ,'Built from 1970-1999'
                                         ,'Built since 2000')))

  return(bldgs)
}



#' acs.rentburden.recode
#'
#' Recodes rent burden info, as from table B25070 ("GROSS RENT AS A PERCENTAGE
#' OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS"). Was developed for 2019 ACS;
#' could break if they change encodings.
#'
#' Note: from socialexplorer.com: "Units for which no rent is paid and units
#' occupied by households that reported no income or a net loss comprise the
#' category, 'Not computed.'"
#'
#' Universe for this table is Renter-occupied Housing Units.
#'
#' B25070_001	Total B25070_002	Total: Less than 10.0 percent B25070_003	Total:
#' 10.0 to 14.9 percent B25070_004	Total: 15.0 to 19.9 percent B25070_005	Total:
#' 20.0 to 24.9 percent B25070_006	Total: 25.0 to 29.9 percent B25070_007	Total:
#' 30.0 to 34.9 percent B25070_008	Total: 35.0 to 39.9 percent B25070_009	Total:
#' 40.0 to 49.9 percent B25070_010	Total: 50.0 percent or more B25070_011	Total:
#' Not computed
#'
#' @param rentb a vacancy table, as from `multiyr.acs.wrapper(B25004, ...)`.
#' @inheritParams acs.demographic.recode
#'
#' @export acs.rentburden.recode
acs.rentburden.recode <- function(rentb
                               ,filter.aggregates = T
                               ) {

  if(filter.aggregates)
    rentb <- rentb %>%
      filter( ! var %in% c(1) )

  rentb <- rentb %>%
    mutate(recode = case_when(
      var %in% c(2:6) ~ 'Not rent-burdened (<30% of income)'
      ,var %in% c(7:9) ~ 'Rent-burdened (30-50% of income)'
      ,var %in% c(10) ~ 'Very rent-burdened (>50% of income)'
      ,TRUE ~ label
      )
    )

  # also use factor rentb status
  rentb$recode <- factor(rentb$recode
                           , levels = c(
                              'Not rent-burdened (<30% of income)'
                             ,'Rent-burdened (30-50% of income)'
                             ,'Very rent-burdened (>50% of income)'
                           ))

  return(rentb)
}



# scratch -----------------------------------------------------------------


