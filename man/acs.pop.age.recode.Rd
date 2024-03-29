% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidycensus-recodes-and-helpers.R
\name{acs.pop.age.recode}
\alias{acs.pop.age.recode}
\title{acs.pop.age.recode}
\usage{
acs.pop.age.recode(x, larger.age.bucket.recode = F)
}
\arguments{
\item{larger.age.bucket.recode}{Recodes to larger age buckets, with "prime
working age" population (25-54) separated out, along with 65+ and under 18
and in-betweens.}
}
\description{
Recode table B01001 to remove gender disaggregation and agg to ages.
}
\details{
(B01001 universe is Total Population)
}
