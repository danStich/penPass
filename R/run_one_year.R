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
#' @param p_stillwater Probability of smolts using Stillwater Branch 
#' Penobscot River for downstream migration. By default sampled from
#' \code{\link{stillwater_splits}} based on simulated value of flow
#' at West Enfield Dam.
#' 
#' @param basin_specific_output Logical indicating whether the result 
#' should be returned for the entire watershed (FALSE, default) or for
#' specific basins (TRUE). If FALSE, then a vector of length one is returned,
#' if TRUE, then a dataframe is returned. 
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
    great_works = NA,
    gilman_falls = NA,
    stillwater = NA,
    orono = NA,
    veazie = NA,
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
    PN_sat = 0),
  p_stillwater = NA,
  basin_specific_output = FALSE
){
  
  # Error handling for argument values ----
  if(any(sat < 0) | any(sat > 1)){
    stop("all values of sat must be greater than or equal 
          to zero and less than or equal to 1.")
  }
  
  # Natural mortality per km ----
  # if(is.null(km_surv)){
  #   km_surv <- 1 - penPass::sim_km_mort(
  #     prop_lost_per_km = penPass::mort_per_km$prop_lost_per_km,
  #     n = 1,
  #     prob = penPass::mort_per_km$prob
  #   )
  # }
  
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
  if(is.na(p_stillwater)){
  p_stillwater <- penPass::get_stillwater_use(
    flow = annual_flows[names(downstream)=="west_enfield"])
  }
  
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
  
  # . Mainstem routines by basin ----
  # Mainstem Penobscot receives upper watershed
  river_mat_wpn <- pn_mat
  river_mat_wpn$n_smolts[1] <- wpn_out$smolts_out[nrow(wpn_out)]  
  river_mat_wpn$n_smolts[2:nrow(river_mat_wpn)] <- 0
  
  river_mat_epn <- pn_mat
  river_mat_epn$n_smolts[2] <- epn_out$smolts_out[nrow(epn_out)]  
  river_mat_epn$n_smolts[c(1, 3:nrow(river_mat_epn))] <- 0
  
  river_mat_matt <- pn_mat
  river_mat_matt$n_smolts[2] <- matt_out$smolts_out[nrow(matt_out)]  
  river_mat_matt$n_smolts[c(1:14, 16:nrow(river_mat_matt))] <- 0
  
  river_mat_pisc <- pn_mat
  river_mat_pisc$n_smolts[2] <- pisc_out$smolts_out[nrow(pisc_out)]  
  river_mat_pisc$n_smolts[c(1:38, 40:nrow(river_mat_pisc))] <- 0  
  
  # Split into Stillwater and Mainstem migration groups by basin
  pn_splitted_wpn <- penPass::split_pn(wpn_mat, river_mat_wpn, p_stillwater)
  pn_mat_wpn_s <- pn_splitted_wpn$river_mat_s
  pn_mat_wpn_m <- pn_splitted_wpn$river_mat_m
  
  pn_splitted_epn <- penPass::split_pn(epn_mat, river_mat_epn, p_stillwater)
  pn_mat_epn_s <- pn_splitted_epn$river_mat_s
  pn_mat_epn_m <- pn_splitted_epn$river_mat_m 
  
  pn_splitted_matt <- penPass::split_pn(matt_mat, river_mat_matt, p_stillwater)
  pn_mat_matt_s <- pn_splitted_matt$river_mat_s
  pn_mat_matt_m <- pn_splitted_matt$river_mat_m    
  
  pn_splitted_pisc <- penPass::split_pn(pisc_mat, river_mat_pisc, p_stillwater)
  pn_mat_pisc_s <- pn_splitted_pisc$river_mat_s
  pn_mat_pisc_m <- pn_splitted_pisc$river_mat_m   
  
  pn_splitted_pn <- penPass::split_pn(pn_mat, pn_mat, p_stillwater)
  pn_mat_pn_s <- pn_splitted_pn$river_mat_s
  pn_mat_pn_m <- pn_splitted_pn$river_mat_m     
  
  
  # Run downstream migration module for each migratory group from each basin
  wpn_out_s <- run_downstream_migration_pn(pn_mat_wpn_s, year = year)
  wpn_out_m <- run_downstream_migration_pn(pn_mat_wpn_m, year = year)  
  
  epn_out_s <- run_downstream_migration_pn(pn_mat_epn_s, year = year)
  epn_out_m <- run_downstream_migration_pn(pn_mat_epn_m, year = year) 
  
  matt_out_s <- run_downstream_migration_pn(pn_mat_matt_s, year = year)
  matt_out_m <- run_downstream_migration_pn(pn_mat_matt_m, year = year)   

  pisc_out_s <- run_downstream_migration_pn(pn_mat_pisc_s, year = year)
  pisc_out_m <- run_downstream_migration_pn(pn_mat_pisc_m, year = year)   
    
  pn_out_s <- run_downstream_migration_pn(pn_mat_pn_s, year = year)
  pn_out_m <- run_downstream_migration_pn(pn_mat_pn_m, year = year)
  
  
  # Add the number of outmigrants from each group in each basin
  wpn_smolts_out <- round(sum(wpn_out_s$smolts_out + wpn_out_m$smolts_out), 0)
  epn_smolts_out <- round(sum(epn_out_s$smolts_out + epn_out_m$smolts_out), 0)
  matt_smolts_out <- round(sum(matt_out_s$smolts_out + matt_out_m$smolts_out), 0)
  pisc_smolts_out <- round(sum(pisc_out_s$smolts_out + pisc_out_m$smolts_out), 0)
  pn_smolts_out <- round(sum(pn_out_s$smolts_out + pn_out_m$smolts_out), 0)
  
  if(basin_specific_output == TRUE){
  return(data.frame(
    year = year,
    wpn = wpn_smolts_out,
    epn = epn_smolts_out,
    matt = matt_smolts_out,
    pisc = pisc_smolts_out,
    pn = pn_smolts_out))
  } else {
    return(smolts_out = wpn_smolts_out +
           epn_smolts_out +
           matt_smolts_out +
           pisc_smolts_out + 
           pn_smolts_out)
  }
}

