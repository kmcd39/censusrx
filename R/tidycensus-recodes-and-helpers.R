

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
      filter( ! var %in% c(1, 2, 9, 13:21) )

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


#' q.recode.commute.table.B08006
#'
#' Uses table B08006, SEX OF WORKERS BY MEANS OF TRANSPORTATION TO WORK.
#' Immediately trims the breakdown by sex.
#'
#' @param commutes a pull of table B08006, as from `multiyr.acs.wrapper(...)`.
#' @param separate.carpools whether to keep carpoolers separate
#' @inheritParams acs.demographic.recode
#'
#' @export acs.commute.recode
acs.commute.recode <- function(x
                               ,separate.carpools = F
                               ,filter.aggregates = T
                               #,others.vars = c(16)
) {

  commutes <- x %>%
    filter(var %in% 1:17)

  if(filter.aggregates)
    commutes <- commutes %>%
    filter( ! var %in% c(1,2,4,8) )

  commutes <- commutes %>%
    mutate(recode =
             case_when(
               var %in% c(3) ~ 'Drove alone'
               ,var %in% c(4:7) ~ 'Carpooled'
               ,var %in% c(8:13) ~ 'Public transit'
               ,var %in% c(14:15) ~ 'Active transit'
               ,var %in% c(16) ~ 'Other'
               ,var %in% c(17) ~ 'Work from home'
             )
           ,.after = variable
    )

  if( !separate.carpools )
    commutes <- commutes %>%
    mutate(recode =
             case_when(
               var %in% c(3:7) ~ 'Private Car'
               ,TRUE ~ recode)
    )

  # set factor levels
  commutes$recode <- factor(commutes$recode
                            , levels =
                              c(
                                'Drove alone'
                                ,'Carpooled'
                                ,'Private Car'
                                ,'Public transit'
                                ,'Active transit'
                                ,'Work from home'
                                ,'Other'
                              ))
  return(commutes)
}


#' acs.rentals.by.level.recode
#'
#' Recodes table B25056 (universe: renter-occupied housing units) or table
#' B25061 (universe: Vacant-for-rent and Rented, Not Occupied Housing Units)
#'
#' @export acs.rentals.by.level.recode
acs.rentals.by.level.recode <- function(x
                                        ,acs.tbl = c('B25056', 'B25061')
                                        ,filter.aggregates = T) {

  acs.tbl <- acs.tbl[1]

  if(filter.aggregates &
     acs.tbl == 'B25056')
    x <- x %>% filter(!var %in% c(1,2) )

  if(filter.aggregates &
     acs.tbl == 'B25061')
    x <- x %>% filter(!var %in% 1)


  if(acs.tbl == 'B25056')
    x <- x %>%
      mutate(recode =
               case_when(
                 var %in% c(3:19) ~ 'Below $1,000'
                 ,var %in% c(20:22) ~ '$1,000 - $2,000'
                 ,var %in% c(23:26) ~ 'Above $2,000'
               )
      )
  if(acs.tbl == 'B25061')
    x <- x %>%
      mutate(recode =
               case_when(
                 var %in% c(3:19-1) ~ 'Below $1,000' # the encodings are always 1 lower on this table.
                 ,var %in% c(20:22-1) ~ '$1,000 - $2,000'
                 ,var %in% c(23:26-1) ~ 'Above $2,000'
               )
      )

  x <- x %>%
    mutate(recode =
             factor(
               recode,
               levels = c(
                 'Below $1,000'
                 ,'$1,000 - $2,000'
                 ,'Above $2,000'
               )
             ))

  return(x)
}



#' acs.households.by.income.recode
#'
#' Recodes table B19001, Household Income, universe: all households.
#'
#' @export acs.households.by.income.recode
acs.households.by.income.recode <- function(x
                                            ,filter.aggregates = T) {


  if(filter.aggregates)
    x <- x %>% filter(!var %in% 1)

  x <- x %>%
    mutate(recode =
             case_when(
               var %in% c(2:9) ~ 'Below $40k'
               ,var %in% c(10:13) ~ '$40k - $100k'
               ,var %in% c(14:17) ~ 'Above $100k'
             )
    ) %>%
    mutate(recode = factor(
      recode,
      levels = c(
        'Below $40k'
        ,'$40k - $100k'
        ,'Above $100k'
      )
    ))

  return(x)
}


#' acs.pop.age.recode
#'
#' Recode table B01001 to remove gender disaggregation and agg to ages.
#'
#' (B01001 universe is Total Population)
#'
#' @param larger.age.bucket.recode Recodes to larger age buckets, with "prime
#'   working age" population (25-54) separated out, along with 65+ and under 18
#'   and in-betweens.
#'
#' @export acs.pop.age.recode
acs.pop.age.recode <- function(x
                               ,larger.age.bucket.recode = F) {


  # drop aggs & remove Total: prefix
  ages <- x %>%
    filter(grepl('years', label)) %>%
    mutate(label =
             gsub('^Total: ', '', label))

  # remove gender & remove redundant "years" suffix
  ages <- ages %>%
    mutate(label =
             gsub('^Male: |^Female: ', '', label)) %>%
    mutate(label =
             gsub(' years', '', label))

  # extract var/label for factor order
  lbls <- ages %>%
    select(variable, var, label) %>%
    distinct() %>%
    arrange(var)

  # agg to just age
  ages <- ages %>%
    group_by(year, geoid, label) %>%
    summarise(n = sum(estimate)) %>%
    ungroup() %>%
    mutate(label = factor(
      label,
      levels = unique(lbls$label)
    ))

  if(larger.age.bucket.recode)
    ages <- ages %>%
    mutate(
      recode =
        case_when(
          grepl('Under 5|5 to 9|10 to 14|15 to 17', label) ~ 'Under 18'
          ,grepl('18 and 19|^20|^21|22 to 24', label) ~ '18 to 24'
          ,grepl('55 to 59|60 and 61|62 to 64', label) ~ '55 to 64'
          ,grepl('65 and 66|67 to 69|70 to 74|75 to 79|80 to 84|85 and over', label) ~ 'Over 65'
          ,TRUE ~ '25 to 54'
        )) %>%
    mutate(recode =
             factor(recode,
                    levels = c(
                      'Under 18'
                      ,'18 to 24'
                      ,'25 to 54'
                      ,'55 to 64'
                      ,'Over 65'
                    )))

  return(ages)

}

#' acs.tenure.by.hh.size.recode
#'
#' Recode table B25009 (unv is occupied housing units); separates out tenure
#' from size.
#'
#' @export acs.tenure.by.hh.size.recode
acs.tenure.by.hh.size.recode <- function(x) {

  # remove aggregates, remove "Total: " prefix.
  hhtenure <- x %>%
    filter( grepl('person household$',
                  label))  %>%
    mutate(label =
             gsub('^Total: ', '', label))


  # separate tenure and szie; recode size
  hhtenure <- hhtenure %>%
    tidyr::separate(label, into = c('tenure', 'size')
                    ,sep = ': ') %>%
    mutate(size.recode =
             case_when(
               grepl('^[1-2]', size) ~ '1 or 2 people'
               ,grepl('^[3-7]', size) ~ '3 or more people'
             )
    )

  # abbreviate redundant labels ("occupied" and "household")
  hhtenure <- hhtenure %>%
    mutate(tenure =
             case_when(
               grepl('Owner', tenure) ~ 'Owners'
               ,grepl('Renter', tenure) ~ 'Renters'
             )
    )

  # get factor order
  ordr <- hhtenure %>%
    select(variable, tenure, size, size.recode) %>%
    distinct() %>%
    arrange(variable)

  # apply factor levels
  hhtenure <- hhtenure %>%
    mutate(tenure =
             factor(tenure
                    ,levels = unique(ordr$tenure))
           ,size.recode =
             factor(size.recode
                    ,levels = unique(ordr$size.recode))
    )

  return(hhtenure)
}

# scratch -----------------------------------------------------------------


