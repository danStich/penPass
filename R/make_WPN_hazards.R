#' @title Create survival hazards in West Branch Penobscot River
#' 
#' @description Function used to create natural survival and dam passage 
#' survival hazards for a given `year` in the West Branch Penobscot River from 
#' Stevens et al. (2019).
#' 
#' @param wpn A dataframe matching output of \code{link{make_WPN}}.
#' 
#' @param km_surv Survival per kilometer for free-flowing reaches
#' 
#' @param downstream_passage A vector of dam passage survival probabilities 
#' matching the `downstream` argument in \code{\link{run_one_year}}
#' 
#' @references 
#' Stevens, JR, JF Kocik, and TF Sheehan. 2019. Modeling the impacts of dams and 
#' stocking practices on an endangered Atlantic salmon (Salmo salar) 
#' population in the Penobscot River, Maine, USA. Canadian Journal of Fisheries
#' and Aquatic Sciences 76:1795-1807.
#' 
#' @export
#' 
make_WPN_hazards <- function(wpn, km_surv, downstream_passage){
  
  wpn$hazard[grep("1.1", wpn$huc_collection_segment_or_damname)] <- km_surv

  wpn$hazard[grep("Dam", wpn$huc_collection_segment_or_damname)] <- downstream_passage[1:7]

  wpn$hazard[is.na(wpn$hazard)] <- 1
  
  return(wpn)
  
}

