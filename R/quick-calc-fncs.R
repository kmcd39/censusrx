#' get.cagr.comparison
#'
#' Relatively inflexible function that gets change, percent change, and CAGR
#' across regions and variables. Assumes columns `geoid`, `variable`, and `year`, as from data
#' retrieved from tidycensus with minimal processing (add year and rename tolower).
#'
#' TODO make the columns variable.
#'
#' @param x dataframe with `geoid`, `variable`, and `year` columns.
#'
#' @export get.cagr.comparison
get.cagr.comparison <- function(
    x) {

  x %>%
    arrange(geoid, variable, year) %>%
    group_by(geoid, variable) %>%
    mutate( ch =
              estimate - lag(estimate)
            ,pch =
              ch / lag(estimate)
            ,cagr =
              (
                (estimate / lag(estimate)) ^
                  ( 1 / (year - lag(year)) )
              ) - 1
    ) %>%
    mutate(period =
             paste0(
               lag(year), ' - ', year )
           ,.after = year
    ) %>%
    ungroup() %>%
    filter(!is.na(ch))

}
