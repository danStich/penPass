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
  
  mat$n_smolts[is.na(mat$n_smolts)] <- 0
  
  mat$available_habitat_units_or_segment_length[
    is.na(mat$available_habitat_units_or_segment_length)] <- 1
  
  mat$smolts_out <- mat$n_smolts
  
  mat$mort <- 1 - mat$hazard
  
  mat$smolts_out[1] <- mat$n_smolts[1] * 
    exp(-mat$mort[1] * mat$available_habitat_units_or_segment_length[1])
  
  for(i in 2:nrow(mat)){
    mat$smolts_out[i] <- mat$n_smolts[i] + mat$smolts_out[i - 1] * 
      exp(-mat$mort[i] * mat$available_habitat_units_or_segment_length[i])
  }
  
  return(mat)
  
}

