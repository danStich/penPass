% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_pn.R
\name{split_pn}
\alias{split_pn}
\title{Split lower Penobscot River smolts}
\usage{
split_pn(wpn_mat, epn_mat, matt_mat, pisc_mat, pn_mat, p_stillwater)
}
\arguments{
\item{wpn_mat}{A dataframe matching \code{link{make_WPN_hazards}}.}

\item{epn_mat}{A dataframe matching \code{link{make_EPN_hazards}}.}

\item{matt_mat}{A dataframe matching \code{link{make_Matt_hazards}}.}

\item{pisc_mat}{A dataframe matching \code{link{make_PISC_hazards}}.}

\item{pn_mat}{A dataframe matching \code{link{make_PN_hazards}}.}

\item{p_stillwater}{Probability of using the Stillwater Branch for
migration around Marsh Island.}
}
\description{
Function used to split migrating smolt population
into migratory routes and add number of dams by migration
route through the Penobscot River based on starting
sub_basin in \code{\link{distance_traveled}}.
}
\references{
Stevens, JR, JF Kocik, and TF Sheehan. 2019. Modeling the impacts of dams and 
stocking practices on an endangered Atlantic salmon (Salmo salar) 
population in the Penobscot River, Maine, USA. Canadian Journal of Fisheries
and Aquatic Sciences 76:1795-1807.
}
