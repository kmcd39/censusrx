% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidycensus-recodes-and-helpers.R
\name{acs.bldg.age.recode}
\alias{acs.bldg.age.recode}
\title{acs.bldg.age.recode}
\usage{
acs.bldg.age.recode(bldgs, filter.aggregates = T)
}
\arguments{
\item{filter.aggregates}{If true, filter totals and subtotals.}

\item{vacancy}{a building-age table, as from \code{multiyr.acs.wrapper(B25034, ...)}.}
}
\description{
Recodes demographic info, as from table B25034 Was developed for 2019 ACS;
could break if they change encodings. Note also table B25035_001 is just
median year structure built.
}
\details{
Universe for this table is Housing Units.

B25034_001	Total
B25034_002	Total: Built 2014 or later
B25034_003	Total: Built 2010 to 2013
B25034_004	Total: Built 2000 to 2009
B25034_005	Total: Built 1990 to 1999
B25034_006	Total: Built 1980 to 1989
B25034_007	Total: Built 1970 to 1979
B25034_008	Total: Built 1960 to 1969
B25034_009	Total: Built 1950 to 1959
B25034_010	Total: Built 1940 to 1949
B25034_011	Total: Built 1939 or earlier
}
