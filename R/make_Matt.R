#' @title Create Mattawamkeag River smolt population
#' 
#' @description Function used to create a dataframe containing river segments
#' and sites for stocking and migration as well as dams from built-in data sets,
#' user-specified arguments in \code{\link{run_one_year}}, and output of
#' \code{\link{get_stocking_data}} for a given `year` in the Mattawamkeag
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
make_Matt <- function(stocking, prod, sat){
  
  matt <- penPass::Matt
  
  matt$saturation[!is.na(matt$saturation)] <- sat["Matt_sat"]
  
  matt$n_smolts <- matt$available_habitat_units_or_segment_length *
    prod["Matt_prod"] * matt$saturation
  
  # Stocking
  matt$n_smolts[4] <- stocking$n_stocked[4]
  matt$n_smolts[5] <- stocking$n_stocked[5]
  matt$n_smolts[6] <- stocking$n_stocked[8]
  matt$n_smolts[16] <- stocking$n_stocked[7]
  matt$n_smolts[53] <- stocking$n_stocked[6]

  return(matt)
  
}

