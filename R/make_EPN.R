#' @title Create East Branch Penobscot River smolt population
#' 
#' @description Function used to create a dataframe containing river segments
#' and sites for stocking and migration as well as dams from built-in data sets,
#' user-specified arguments in \code{\link{run_one_year}}, and output of
#' \code{\link{get_stocking_data}} for a given `year` in the East Branch of
#' the Penobscot River from Stevens et al. (2019).
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
make_EPN <- function(stocking, prod, sat){
  
  epn <- penPass::EPN
  
  epn$saturation[!is.na(epn$saturation)] <- sat["EPN_sat"]
  
  epn$n_smolts <- epn$available_habitat_units_or_segment_length *
    prod["EPN_prod"] * epn$saturation
  
  # Stocking
  epn$n_smolts[2] <- stocking$n_stocked[3]
  epn$n_smolts[33] <- stocking$n_stocked[2]
  
  return(wpn)
  
}

