#' @title Create Piscataquis River smolt population
#' 
#' @description Function used to create a dataframe containing river segments
#' and sites for stocking and migration as well as dams from built-in data sets,
#' user-specified arguments in \code{\link{run_one_year}}, and output of
#' \code{\link{get_stocking_data}} for a given `year` in the Piscataquis
#' River from Stevens et al. (2019).
#' 
#' @param stocking A dataframe matching output of 
#' \code{link{get_stocking_data}}.
#' 
#' @param prod A named vector matching the `prod` argument specified in 
#' \code{\link{run_one_year}}
#' 
#' @param sat A named vector matching the `sat` argument specified in 
#' \code{\link{run_one_year}}
#' 
#' @references 
#' Stevens, JR, JF Kocik, and TF Sheehan. 2019. Modeling the impacts of dams and 
#' stocking practices on an endangered Atlantic salmon (Salmo salar) 
#' population in the Penobscot River, Maine, USA. Canadian Journal of Fisheries
#' and Aquatic Sciences 76:1795-1807.
#' 
#' @export
#' 
make_PISC <- function(stocking, prod, sat){
  
  pisc <- penPass::PISC
  
  pisc$saturation[!is.na(pisc$saturation)] <- sat["PISC_sat"]
  
  pisc$n_smolts <- pisc$available_habitat_units_or_segment_length *
    prod["PISC_prod"] * pisc$saturation
  
  # Stocking
  pisc$n_smolts[12] <- stocking$n_stocked[15]
  pisc$n_smolts[22] <- stocking$n_stocked[14]
  pisc$n_smolts[25] <- stocking$n_stocked[13]
  pisc$n_smolts[30] <- stocking$n_stocked[17]
  pisc$n_smolts[31] <- stocking$n_stocked[16]
  pisc$n_smolts[44] <- stocking$n_stocked[12]
  pisc$n_smolts[45] <- stocking$n_stocked[11]
  pisc$n_smolts[46] <- stocking$n_stocked[10]
  pisc$n_smolts[52] <- stocking$n_stocked[9] 
  
  return(pisc)
  
}

