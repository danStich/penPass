#' @title Run one year in penPass model
#' 
#' @description Function used to simulate downstream migration in a single
#' year for available timeseries (1970 - 2020). A wrapper for the `sample()` function for resampling the empirical 
#' cumulative distribution function included in the \code{\link{mort_per_km}}
#' data set.
#' 
#' @param year Year used for simulation. Must be 1970 - 2010.
#' 
#' @param downstream Downstream dam passage survival rates for smolts
#' 
#' @param km_surv Natural downstream survival of smolts per kilometer
#' 
#' @param prod Habitat productivity in smolts produced per 100 m2 of
#' wetted habitat area (Stevens et al. 2019).
#' 
#' @param sat Habitat saturation for wild fish as the proportion of 
#' habitat filled
#' 
#' @references 
#' Stevens, JR, JF Kocik, and TF Sheehan. 2019. Modeling the impacts of dams and 
#' stocking practices on an endangered Atlantic salmon (Salmo salar) 
#' population in the Penobscot River, Maine, USA. Canadian Journal of Fisheries
#' and Aquatic Sciences 76:1795-1807.
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
    km_surv <- 1 - penPass::sim_km_mort(
      prop_lost_per_km = penPass::mort_per_km$prop_lost_per_km,
      n = 1,
      prob = penPass::mort_per_km$prob
    )
  }
  
  # Annual flows by dam where applicable ----
  annual_flows <- penPass::get_annual_flows(year = year)
  
  # Downstream passage at dams ----
  # . User-specified values ----
  # Get user-specified or default downstream survival samples
  # from Stevens et al. (2019)
  # Make the user-specified list into a vector
  downstream_passage <- unlist(downstream) 
  
  # . Default values ----
  dam_survival <- penPass::get_dam_passage(year = year, flow = annual_flows)
  
  # Replace any NA values with the flow-correlated survival values
  downstream_passage[is.na(downstream_passage)] <- dam_survival[is.na(downstream_passage)]
  # names(downstream_passage) <- NULL
  
  # Stillwater use module ----
  p_stillwater <- penPass::get_stillwater_use(
    flow = annual_flows[names(downstream)=="west_enfield"])
  
  # Stocking data ----
  stocking <- penPass::get_stocking_data(year = year)
  
  # Build watershed components ----
  wpn <- penPass::make_WPN(stocking, prod, sat)
  epn <- penPass::make_EPN(stocking, prod, sat)
  matt <- penPass::make_Matt(stocking, prod, sat)
  pisc <- penPass::make_PISC(stocking, prod, sat)
  pn <- penPass::make_PN(stocking, prod, sat)
  
  # Downstream migration hazards ----
  wpn_mat <- penPass::make_WPN_hazards(wpn, km_surv, downstream_passage) 
  epn_mat <- penPass::make_EPN_hazards(epn, km_surv, downstream_passage) 
  matt_mat <- penPass::make_Matt_hazards(matt, km_surv, downstream_passage) 
  pisc_mat <- penPass::make_PISC_hazards(pisc, km_surv, downstream_passage) 
  pn_mat <- penPass::make_PN_hazards(pn, km_surv, downstream_passage) 

  # Downstream migration module ----
  # . Tributaries ----
  wpn_out <- penPass::run_downstream_migration(wpn_mat)
  epn_out <- penPass::run_downstream_migration(epn_mat)
  matt_out <- penPass::run_downstream_migration(matt_mat)
  pisc_out <- penPass::run_downstream_migration(pisc_mat)
  
  # . Mainstem ----
  # Mainstem Penobscot receives upper watershed
  pn_mat$n_smolts[1] <- wpn_out$smolts_out[nrow(wpn_out)]
  pn_mat$n_smolts[2] <- epn_out$smolts_out[nrow(epn_out)]
  pn_mat$n_smolts[15] <- matt_out$smolts_out[nrow(matt_out)]
  pn_mat$n_smolts[39] <- pisc_out$smolts_out[nrow(pisc_out)]

  # Split into Stillwater and Mainstem migration groups
  pn_splitted <- penPass::split_pn(wpn_mat, 
                                   epn_mat, 
                                   matt_mat, 
                                   pisc_mat, 
                                   pn_mat,
                                   p_stillwater
                                   )
  
  pn_mat_s <- pn_splitted$pn_mat_s
  pn_mat_m <- pn_splitted$pn_mat_m
  
  # Run downstream migration module for each group
  pn_out_s <- run_downstream_migration_pn(pn_mat_s, year = year)
  pn_out_m <- run_downstream_migration_pn(pn_mat_m, year = year)
  
  # Apply latent estuary mortality from dams
  smolts_out <- round(sum(pn_out_s$smolts_out + pn_out_m$smolts_out), 0)
  
  return(smolts_out)
  
}

