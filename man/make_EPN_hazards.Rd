% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_EPN_hazards.R
\name{make_EPN_hazards}
\alias{make_EPN_hazards}
\title{Create survival hazards in East Branch Penobscot River}
\usage{
make_EPN_hazards(epn, km_surv, downstream_passage)
}
\arguments{
\item{epn}{A dataframe matching output of \code{link{make_EPN}}.}

\item{km_surv}{Survival per kilometer for free-flowing reaches}

\item{downstream_passage}{A vector of dam passage survival probabilities 
matching the `downstream` argument in \code{\link{run_one_year}}}
}
\description{
Function used to create natural survival and dam passage 
survival hazards for a given `year` in the East Branch Penobscot River from 
Stevens et al. (2019).
}
\references{
Stevens, JR, JF Kocik, and TF Sheehan. 2019. Modeling the impacts of dams and 
stocking practices on an endangered Atlantic salmon (Salmo salar) 
population in the Penobscot River, Maine, USA. Canadian Journal of Fisheries
and Aquatic Sciences 76:1795-1807.
}
