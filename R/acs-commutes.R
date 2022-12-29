#' q.recode.commute.table.B08006
#'
#' Uses table B08006, SEX OF WORKERS BY MEANS OF TRANSPORTATION TO WORK.
#' Immediately trims the breakdown by sex.
#'
#' @param commutes a pull of table B08006, as from `multiyr.acs.wrapper(...)`.
#' @param separate.carpools whether to keep carpoolers separate
#' @inheritParams acs.demographic.recode
#'
#' @export q.recode.commute.table.B08006
q.recode.commute.table.B08006 <- function(x
                                          ,separate.carpools = F
                                          ,filter.aggregates = T) {

  commutes <- x %>%
    filter(var %in% 1:17)

  if(filter.aggregates)
    commutes <- commutes %>%
      filter( ! var %in% c(1,2,4,8) )

  #if(drop.obscure) commutes <- commutes %>%  filter( ! var %in% c(16) )

  commutes <- commutes %>%
    mutate(recode = case_when(
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
    mutate(recode = case_when(
      var %in% c(3:7) ~ 'Car'
      ,TRUE ~ recode    )
    )

  # also use factor commutes status
  commutes$recode <- factor(commutes$recode
                            , levels =
                              c(
                                'Drove alone'
                                ,'Carpooled'
                                ,'Car'
                                ,'Public transit'
                                ,'Active transit'
                                ,'Work from home'
                                ,'Other'
                              ))

  commutes <- commutes %>%
    group_by(geoid, name, recode) %>%

    return(commutes)
}

# commute mode shares -----------------------------------------------------

#' quick.pull.commute.share
#'
#' Wrapper to pull and recode table B08006, SEX OF WORKERS BY MEANS OF TRANSPORTATION TO WORK.
#' Immediately trims the breakdown by sex.
#'
#' @param ... passed onto `tidycensus::get_acs`
#' @param separate.carpools whether to keep carpoolers separate
#' @inheritParams q.recode.commute.table.B08006
#'
#' @export quick.pull.commute.share
quick.pull.commute.share <- function( ...
                                      ,separate.carpools = F
                                      ,filter.aggregates = T) {

  #browser()

  commutes <-  tidycensus::get_acs(
    ...
    ,table = 'B08006'
    ,cache_table = T
  ) %>%
    rename_with( tolower ) %>%
    mutate(var = censusrx::extract.acs.var(variable),
           .after = variable)

  tots <- commutes %>%
    filter(var == 1) %>%
    select(geoid, tot.workers = estimate)

  commutes <- q.recode.commute.table.B08006(
    commutes
    ,separate.carpools = separate.carpools
    ,filter.aggregates = filter.aggregates
  )

  # add totals
  commutes <- commutes %>%
    left_join(tots)

  # sum to recode category, add modeshare
  commutes <- commutes %>%
    group_by(geoid, name, tot.workers, recode ) %>%
    summarise(n = sum(estimate)) %>%
    ungroup() %>%
    mutate(perc = n / tot.workers)

  return(commutes)
}


# commute car and transit times -----------------------------------------------------------


#' commute.times.by.mode
#'
#' Rigid function: outputs a table for counties or other (larger) areas in a
#' state with modeshare and avg. commute times for bus/car/trains/and total
#' transit.
#'
#' @param geo geography type, passed to tidycensus. Default is county. Not
#'   not all geographies available for this pull.
#' @param state state fp or abrv.
#' @param year year
#'
#' @export commute.times.by.mode
commute.times.by.mode <- function( geo = 'county'
                                         ,state
                                         ,year = 2019) {


  # browser()
  # acs table B08136 - AGGREGATE TRAVEL TIME TO WORK (IN MINUTES) OF WORKERS BY
  # MEANS OF TRANSPORTATION TO WORK
  cmts <- tidycensus::get_acs(
    geography = geo
    ,table = 'B08136'
    ,year = year
    #,county = cofps
    ,state = state
    ,cache_table = T
  ) %>%
    rename_with( tolower ) %>%
    select(-name)

  cmt <- cmts %>%
    mutate(var = censusrx::extract.acs.var(variable)) %>%
    mutate(geoid = geox::fix.geoid(geoid))

  # TABLE ENCODING:
  #
  # B08136_001	Aggregate travel time to work (in minutes)
  # B08136_002	Aggregate travel time to work (in minutes): Car, truck, or van
  # B08136_003	Aggregate travel time to work (in minutes): Car, truck, or van: Drove alone
  # B08136_004	Aggregate travel time to work (in minutes): Car, truck, or van: Carpooled
  # B08136_005	Aggregate travel time to work (in minutes): Car, truck, or van: Carpooled: In 2-person carpool
  # B08136_006	Aggregate travel time to work (in minutes): Car, truck, or van: Carpooled: In 3-or-more-person carpool
  # B08136_007	Aggregate travel time to work (in minutes): Public transportation (excluding taxicab)
  # B08136_008	Aggregate travel time to work (in minutes): Public transportation (excluding taxicab): Bus
  # B08136_009	Aggregate travel time to work (in minutes): Public transportation (excluding taxicab): Subway or elevated rail, Light rail, streetcar, or trolley (carro pÃƒÂºblico in Puerto Rico)
  # B08136_010	Aggregate travel time to work (in minutes): Public transportation (excluding taxicab): Long-distance train or commuter rail or Ferryboat
  # B08136_011	Aggregate travel time to work (in minutes): Walked
  # B08136_012	Aggregate travel time to work (in minutes): Taxicab, motorcycle, bicycle, or other means
  #
  # We want All car vs public trans breakout


  # recode (note the tables are grouped differently; so will only do car and
  # transit -- it's what's relevant anyway.)

  # filter to just car and transit
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
    summarise(agg.mins = sum(estimate, na.rm = T))

  # join total commuters - table B08006

  #B08006_001	Total
  # B08006_002	Total: Car, truck, or van
  # B08006_003	Total: Car, truck, or van: Drove alone
  # B08006_004	Total: Car, truck, or van: Carpooled
  # B08006_005	Total: Car, truck, or van: Carpooled: In 2-person carpool
  # B08006_006	Total: Car, truck, or van: Carpooled: In 3-person carpool
  # B08006_007	Total: Car, truck, or van: Carpooled: In 4-or-more-person carpool
  # B08006_008	Total: Public transportation (excluding taxicab)
  # B08006_009	Total: Public transportation (excluding taxicab): Bus
  # B08006_010	Total: Public transportation (excluding taxicab): Subway or elevated rail
  # B08006_011	Total: Public transportation (excluding taxicab): Long-distance train or commuter rail
  # B08006_012	Total: Public transportation (excluding taxicab): Light rail, streetcar or trolley (carro pÃƒÂºblico in Puerto Rico)
  # B08006_013	Total: Public transportation (excluding taxicab): Ferryboat
  # B08006_014	Total: Bicycle
  # B08006_015	Total: Walked
  cmodes <-
    tidycensus::get_acs(
      geography = geo
      ,table = 'B08006'
      ,year = 2019
      #,county = cofps
      ,state = state
      ,cache_table = T
    ) %>%
    rename_with( tolower ) %>%
    select(-name)

  cmode <- cmodes %>%
    mutate(var = censusrx::extract.acs.var(variable)) %>%
    mutate(geoid = geox::fix.geoid(geoid))

  # add total, using total var in acs (not summing by area)
  cmode <- cmode %>%
    group_by(geoid) %>%
    mutate(tot.commuters =
             cur_data()[cur_data()$variable == 'B08006_001',]$estimate)

  # recode similarly
  cmode <- cmode %>%
    filter(var %in% c(2,8:12)) %>%
    group_by(geoid) %>%
    mutate(recode =
             case_when(
               var %in% c(2) ~ 'Car'
               ,var %in% 8 ~ 'Public transit (total)'
               ,var %in% 9 ~ 'Bus'
               ,var %in% c(10:12) ~ 'Train/subway'
               ,TRUE ~ 'Other') # label)
    ) %>%
    group_by(geoid, recode, tot.commuters) %>%
    summarise(n.commuters = sum(estimate, na.rm = T)) %>%
    group_by(geoid) %>%
    mutate(mode.perc =
             n.commuters /
             tot.commuters) %>%
    ungroup()

  cmt <- cmode %>%
    left_join(cmt) %>%
    mutate(avg.mins = agg.mins / n.commuters)

  return(cmt)

}


