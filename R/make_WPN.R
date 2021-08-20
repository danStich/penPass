#' @title Create West Branch Penobscot River smolt population
#' 
#' @description Function used to create a dataframe containing river segments
#' and sites for stocking and migration as well as dams from built-in data sets,
#' user-specified arguments in \code{\link{run_one_year}}, and output of
#' \code{\link{get_stocking_data}} for a given `year` in the West Branch 
#' Penobscot River from Stevens et al. (2019).
#' 
#' @param stocking A dataframe matching output of \code{link{get_stocking_data}}.
#' Not implemented in WPN right now because no fish were historically stocked
#' in this watershed segment (see \code{link{stocking_data}}).
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
make_WPN <- function(stocking, prod, sat){
  
  wpn <- penPass::WPN
  
  wpn$saturation[!is.na(wpn$saturation)] <- sat["WPN_sat"]
  
  wpn$n_smolts <- wpn$available_habitat_units_or_segment_length *
    prod["WPN_prod"] * wpn$saturation
  
  return(wpn)
  
}

