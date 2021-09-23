#' @title Run downstream migration module
#' 
#' @description Function used to create natural survival and dam passage 
#' survival hazards for a given `year` in the East Branch Penobscot River from 
#' Stevens et al. (2019).
#' 
#' @param mat A dataframe matching output of \code{link{make_WPN_hazard}},
#' \code{link{make_EPN_hazard}}, \code{link{make_Matt_hazard}}, 
#' \code{link{make_PISC_hazard}} or \code{link{make_PN_hazard}}.
#' 
#' @references 
#' Stevens, JR, JF Kocik, and TF Sheehan. 2019. Modeling the impacts of dams and 
#' stocking practices on an endangered Atlantic salmon (Salmo salar) 
#' population in the Penobscot River, Maine, USA. Canadian Journal of Fisheries
#' and Aquatic Sciences 76:1795-1807.
#' 
#' @export
#' 
run_downstream_migration <- function(mat){
  
  # Replace NA values with zeros for number of smolts starting in 
  # each collection unit or segment
  mat$n_smolts[is.na(mat$n_smolts)] <- 0
  
  # Make "reach lengths" of 0 so we can ignore collections segments
  # during downstream migration.
  mat$available_habitat_units_or_segment_length[
    is.na(mat$available_habitat_units_or_segment_length)] <- 0
  
  # Make container to hold number of smolts making it out
  mat$smolts_out <- mat$n_smolts
  
  # Get mortality per km
  mat$mort <- 1 - mat$hazard

  # Calculate whole-reach mortality based on reach km and 
  mat$s_reach <- exp(-mat$mort[1:nrow(mat)] * mat$available_habitat_units_or_segment_length[1:nrow(mat)])
  
  mat$smolts_out <- mat$n_smolts
  mat$smolts_out[1] <- mat$n_smolts[1] * dplyr::last(cumprod(mat$s_reach[1:nrow(mat)]))
  
  for(i in 2:nrow(mat)){

  mat$smolts_out[i] <- mat$n_smolts[i] * dplyr::last(cumprod(mat$s_reach[i:nrow(mat)]))    
    
  }
  
  return(mat)
  
}

