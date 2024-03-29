#' @title Create survival hazards in East Branch Penobscot River
#' 
#' @description Function used to create natural survival and dam passage 
#' survival hazards for a given `year` in the East Branch Penobscot River from 
#' Stevens et al. (2019).
#' 
#' @param epn A dataframe matching output of \code{link{make_EPN}}.
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
make_EPN_hazards <- function(epn, km_surv, downstream_passage){
  
  epn$hazard <- NA
  
  if(is.null(km_surv)){
      epn$hazard[c(grep("1.2", epn$huc_collection_segment_or_damname))] <- 
        1 - penPass::sim_km_mort(prop_lost_per_km = penPass::mort_per_km$prop_lost_per_km,
        n = length(epn$hazard[grep("1.2", epn$huc_collection_segment_or_damname)]),
        prob = penPass::mort_per_km$prob)
  } else {

  epn$hazard[grep("1.2", epn$huc_collection_segment_or_damname)] <- km_surv

  }

  epn$hazard[grep("Dam", epn$huc_collection_segment_or_damname)] <- downstream_passage[8]

  epn$hazard[is.na(epn$hazard)] <- 1
  
  return(epn)
  
}

