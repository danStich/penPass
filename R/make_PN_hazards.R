#' @title Create survival hazards in lower mainstem Penobscot River
#' 
#' @description Function used to create natural survival and dam passage 
#' survival hazards for a given `year` in the mainstem Penobscot River from 
#' Stevens et al. (2019).
#' 
#' @param pn A dataframe matching output of \code{link{make_PN}}.
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
make_PN_hazards <- function(pn, km_surv, downstream_passage){
  
  pn$hazard[grep("1M", pn$huc_collection_segment_or_damname)] <- km_surv
  pn$hazard[grep("1S", pn$huc_collection_segment_or_damname)] <- km_surv

  pn$hazard[grep("Dam", pn$huc_collection_segment_or_damname)] <- 
    downstream_passage[13:21]
  
  pn$hazard[is.na(pn$hazard)] <- 1
  
  return(pn)
  
}

