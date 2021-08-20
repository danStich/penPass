#' @title Run one year in penPass model
#' 
#' @description Function used to simulate downstream migration in a single
#' year for available timeseries (1970 - 2020). A wrapper for the `sample()` function for resampling the empirical 
#' cumulative distribution function included in the \code{\link{mort_per_km}}
#' data set.
#' 
#' @param year Year used for simulation. Must be 1970 - 2020.
#' 
#' @param downstream Downstream dam passage survival rates for smolts
#' 
#' @param km_surv Natural downstream survival of smolts per kilometer
#' 
#' @export
#' 
run_one_year <- function(
  year = 2017,
  downstream = list(
    seeboomook = NA,
    ripogenus = NA,
    north_twin = NA,
    quakish = NA,
    dolby = NA,
    east_millinocket = NA,
    medway = NA,
    matagamon = 1,
    guilford = NA,
    moosehead = NA,
    browns_mills = NA,
    howland = NA,
    mattaseunk = NA,
    west_enfield = NA,
    milford = NA,
    great_works = 1,
    gilman_falls = NA,
    stillwater = NA,
    orono = NA,
    veazie = 1,
    bangor_waterworks = 1
  ),
  km_surv = NULL,
  prod = c(
    WPN_prod = 3, 
    EPN_prod = 3, 
    Matt_prod = 3, 
    PISC_prod = 3,
    PN_prod = 3),
  sat = c(
    WPN_sat = 0, 
    EPN_sat = 0, 
    Matt_sat = 0, 
    PISC_sat = 0,
    PN_sat = 0)
  
){
  
  # Error handling for argument values ----
  if(any(sat < 0) | any(sat > 1)){
    stop("all values of sat must be greater than or equal 
          to zero and less than or equal to 1.")
  }
  
  # Natural mortality per km ----
  if(is.null(km_surv)){
    km_surv <- 1 - sim_km_mort(
      prop_lost_per_km = penPass::mort_per_km$prop_lost_per_km,
      n = 1,
      prob = penPass::mort_per_km$prob
    )
  }
  
  # Annual flows by dam where applicable ----
  annual_flows <- get_annual_flows(year = year)
  
  # Downstream passage at dams ----
  # . User-specified values ----
  # Get user-specified or default downstream survival samples
  # from Stevens et al. (2019)
  # Make the user-specified list into a vector
  downstream_passage <- unlist(downstream) 
  
  # . Default values ----
  dam_survival <- get_dam_passage(year = year, flow = annual_flows)
  
  # Replace any NA values with the flow-correlated survival values
  downstream_passage[is.na(downstream_passage)] <- dam_survival[is.na(downstream_passage)]
  names(downstream_passage) <- NULL
  
  # Stillwater use module ----
  p_stillwater <- get_stillwater_use(
    flow = annual_flows[names(downstream)=="west_enfield"])
  
  # Stocking data ----
  stocking <- get_stocking_data(year = year)
  
  # Build watershed components ----
  # West Branch Penobscot River
  wpn <- make_WPN(stocking, prod, sat)
  # East Branch Penobscot River
  epn <- make_EPN(stocking, prod, sat)
  # Mattawamkeag River
  matt <- make_Matt(stocking, prod, sat)
  # Piscataquis River
  pisc <- make_PISC(stocking, prod, sat)
  # Penobscot River
  pn <- make_PN(stocking, prod, sat)
  
}

