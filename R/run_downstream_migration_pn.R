#' @title Run downstream migration module
#' 
#' @description Function used to run downstream migration for lower mainstem
#' Penobscot River based on output of \code{link{make_WPN_hazard}},
#' \code{link{make_EPN_hazard}}, \code{link{make_Matt_hazard}}, and
#' \code{link{make_PISC_hazard}} based on Stevens et al. (2019).
#' 
#' @param mat A dataframe matching \code{link{make_PN_hazard}}
#' 
#' @param year Year of simulation
#' 
#' @references 
#' Stevens, JR, JF Kocik, and TF Sheehan. 2019. Modeling the impacts of dams and 
#' stocking practices on an endangered Atlantic salmon (Salmo salar) 
#' population in the Penobscot River, Maine, USA. Canadian Journal of Fisheries
#' and Aquatic Sciences 76:1795-1807.
#' 
#' @export
#' 
run_downstream_migration_pn <- function(mat, year){
  

  # Latent dam mortality in estuary ----
  # Remove dams depending on year 
  if(year >= 1968){mat$n_dams[104] <- 0}
  if(year >= 2012){mat$n_dams[68] <- 0}
  if(year >= 2013){mat$n_dams[98] <- 0}
  
  # Calculate number of dams experienced by each row of mat
  for(i in 1:nrow(mat)){
    mat$cum_dams[i] <- dplyr::last(cumsum(mat$n_dams[i:nrow(mat)])) +
      mat$conf_dams[i]
  }
  
  # Maximum number of dams reduced to 8 for estuary module
  mat$cum_dams[mat$cum_dams > 8] <- 8
  
  # Draw latent estuary mortality coeff from mean and sd of coefficients
  # reported by Stich et al. (2015)
  est_mort_coeff <- rnorm(1, -0.376, 0.092)
  
  est_mort_0 <- rnorm(1, 0.87, 0.0125)

  # Align either the coeff or est_mort_0 with each habitat unit
  # based on cumulative number of dams passed.
  
  # Fill with stochastic value for zero dams by default
  mat$estuary_s <- est_mort_0
  
  # If one or more dams passed, then multiply the coefficient 
  # by the standardized number of dams and invert logit function
  for(i in 1:nrow(mat)){
    if(mat$cum_dams[i] > 0){
    
      # Get standardized number of dams from built-in dataset
      std_dams <- 
        penPass::latent_estuary$n_dams_std[
          penPass::latent_estuary$n_dams == mat$cum_dams[i]]
      
      # Multiply by coeff and invert the logit
      mat$estuary_s[i] <- exp(std_dams * est_mort_coeff) /
        (1 + exp(std_dams * est_mort_coeff))
    
    }
  }
  
  # Downstream survival module ----
  # Replace NA values with zeros for number of smolts starting in 
  # each collection unit or segment
  mat$n_smolts[is.na(mat$n_smolts)] <- 0
  
  # Make "reach lengths" of 0 so we can ignore collections segments
  # during downstream migration.
  mat$available_habitat_units_or_segment_length[
    is.na(mat$available_habitat_units_or_segment_length)] <- 0
    
  
  # Get mortality per km in FW - need to do estuary inside of 
  # loop since it varies by reach
  mat$mort <- 1 - mat$hazard

  # Calculate whole-reach mortality based on reach km and 
  mat$s_reach <- exp(
    -mat$mort[1:nrow(mat)] * 
      mat$available_habitat_units_or_segment_length[1:nrow(mat)]
    )
  
  # Calculate number of smolts leaving
  mat$smolts_out <- mat$n_smolts
  

  for(i in 1:nrow(mat)){

    estuary_mort <- 1 - mat$estuary_s[i]
    estuary_mort_km <- estuary_mort/38
    
    mat$s_reach[99:125] <- 1
    
    mat$s_reach[99:125][grep("1M", 
                            mat$huc_collection_segment_or_damname[99:125])] <- 
    exp(
    -estuary_mort_km * 
      mat$available_habitat_units_or_segment_length[99:125][grep("1M", 
                            mat$huc_collection_segment_or_damname[99:125])]
    ) 
    
    mat$smolts_out[i] <- mat$n_smolts[i] * 
      dplyr::last(cumprod(mat$s_reach[i:nrow(mat)]))    
    
  }
  
  return(mat)
  
}

