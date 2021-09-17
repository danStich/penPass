#' @title Run downstream migration module
#' 
#' @description Function used to run downstream migration for lower mainstem
#' Penobscot River based on output of \code{link{make_WPN_hazard}},
#' \code{link{make_EPN_hazard}}, \code{link{make_Matt_hazard}}, and
#' \code{link{make_PISC_hazard}} based on Stevens et al. (2019).
#' 
#' @param mat A dataframe matching \code{link{make_PN_hazard}}.
#' 
#' @references 
#' Stevens, JR, JF Kocik, and TF Sheehan. 2019. Modeling the impacts of dams and 
#' stocking practices on an endangered Atlantic salmon (Salmo salar) 
#' population in the Penobscot River, Maine, USA. Canadian Journal of Fisheries
#' and Aquatic Sciences 76:1795-1807.
#' 
#' @export
#' 
run_downstream_migration_pn <- function(mat){
  
  mat$n_smolts[is.na(mat$n_smolts)] <- 0
  
  mat$available_habitat_units_or_segment_length[
    is.na(mat$available_habitat_units_or_segment_length)] <- 1
  
  mat$smolts_out <- mat$n_smolts
  
  mat$mort <- 1 - mat$hazard
  
  mat$s_reach <- exp(-mat$mort[i:nrow(mat)] * mat$available_habitat_units_or_segment_length[i:nrow(mat)])
  
  pn_out <- vector(mode = "numeric", length = nrow(mat))
  
  
  for(i in 2:nrow(mat)){

  pn_out[i] <- mat$n_smolts[i] * dplyr::last(cumprod(mat$s_reach[i:nrow(mat)]))    
    
  }
  
  return(mat$smolts_out)
  
}

