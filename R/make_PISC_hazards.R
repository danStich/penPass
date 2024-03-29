#' @title Create survival hazards in Piscataquis River
#' 
#' @description Function used to create natural survival and dam passage 
#' survival hazards for a given `year` in the Piscataquis River from 
#' Stevens et al. (2019).
#' 
#' @param pisc A dataframe matching output of \code{link{make_PISC}}.
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
make_PISC_hazards <- function(pisc, km_surv, downstream_passage){
  
  pisc$hazard <- NA
  
  if(is.null(km_surv)){
      pisc$hazard[c(grep("1.4", pisc$huc_collection_segment_or_damname))] <- 
        1 - penPass::sim_km_mort(prop_lost_per_km = penPass::mort_per_km$prop_lost_per_km,
        n = length(pisc$hazard[grep("1.4", pisc$huc_collection_segment_or_damname)]),
        prob = penPass::mort_per_km$prob)
  } else {

  pisc$hazard[grep("1.4", pisc$huc_collection_segment_or_damname)] <- km_surv

  }
  
  pisc$hazard[grep("Dam", pisc$huc_collection_segment_or_damname)] <- downstream_passage[9:12]

  pisc$hazard[is.na(pisc$hazard)] <- 1

  
  return(pisc)
  
}

