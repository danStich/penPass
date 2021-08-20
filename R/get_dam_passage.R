#' @title Get dam-specific survival rates
#' 
#' @description Get correlated dam-specific downstream survival rates for 
#' smolts at each dam from built-in `downstream_` datasets or default values
#' based on triangular distributions or fixed rates used in Stevens et al. 
#' (2019). Values based on Amaral et al. (2012) and sampled following 
#' Nieland et al. (2013, 2015) and Nieland and Sheehan (2020), sampled using 
#' `EnvStats::rtri()`, or fixed to 1 for other dams based on 
#' Stevens et al. (2019).
#' 
#' @param flow A numeric vector of length 21 containing flow values (cfs) 
#' corresponding to dams in rows in \code{\link{penPass::flow_ratios}} and
#' the `upstream` argument of \code{\link{run_one_year}}.
#' 
#' @return A vector with dam passage rates 
#' 
#' @references
#' Amaral S, Fay C, Hecker G, Perkins N. 2012. Atlantic salmon survival 
#' estimates at mainstem hydroelectric projects on the Penobscot River. 
#' Phase 3 Final Report. Alden Research Laboratory, Inc., Holden, MA.
#' 
#' Nieland JL, Sheehan TF. 2020. Quantifying the Effects of Dams on Atlantic Salmon
#' in the Penobscot River Watershed, with a Focus on Weldon Dam. US Department of 
#' Commerce, Northeast Fisheries Science Center Reference Document 19-16, Woods 
#' Hole, MA.
#' 
#' Nieland JL, Sheehan TF, Saunders R. 2015. Assessing demographic effects of dams
#' on diadromous fish: a case study for Atlantic salmon in the Penobscot River, 
#' Maine. ICES Journal of Marine Science 72:2423–2437. 
#' 
#' Nieland JL, Sheehan TF, Saunders R, Murphy JS, Trinko Lake TR, Stevens JR. 2013. 
#' Dam Impact Analysis model for Atlantic salmon in the Penobscot River, Maine. US 
#' Department of Commerce, Northeast Fisheries Science Center Reference Document 
#' 13-09, Woods Hole, MA.
#' 
#' Stevens, JR, JF Kocik, and TF Sheehan. 2019. Modeling the impacts of dams and 
#' stocking practices on an endangered Atlantic salmon (Salmo salar) 
#' population in the Penobscot River, Maine, USA. Canadian Journal of Fisheries
#' and Aquatic Sciences 76:1795-1807.
#' @export
#' 
get_dam_passage <- function(year = 2020, flow){
  
    # Extract correlated flow-contingent survival rates
    dam_survival <- c(
      EnvStats::rtri(1, min = 0.50, max = 0.90, mode = 0.80), # Seeboomook
      EnvStats::rtri(1, min = 0.50, max = 0.90, mode = 0.80), # Ripogenus
      EnvStats::rtri(1, min = 0.50, max = 0.90, mode = 0.80), # North Twin
      EnvStats::rtri(1, min = 0.50, max = 0.90, mode = 0.80), # Quakish
      EnvStats::rtri(1, min = 0.50, max = 0.90, mode = 0.80), # Dolby
      EnvStats::rtri(1, min = 0.50, max = 0.90, mode = 0.80), # East Millinocket
      
      # Medway
      ifelse(
        max(penPass::downstream_medway >= flow[7]),
        penPass::downstream_medway[min(which(
          penPass::downstream_medway$flow_cfs >= flow[7])),4],
        1
      ),
      
      # Matagamon
      EnvStats::rtri(1, min = 0.50, max = 0.90, mode = 0.80), 
      
      # Guilford
      rnorm(n = 1, mean = 0.968, sd = 0.007),                 
      
      # Moosehead
      ifelse(
        max(penPass::downstream_moosehead >= flow[7]),
        penPass::downstream_moosehead[min(which(
          penPass::downstream_moosehead$flow_cfs >= flow[7])),4],
        1
      ),
      
      # Brown's Mills
      ifelse(
        max(penPass::downstream_brownsmills >= flow[7]),
        penPass::downstream_brownsmills[min(which(
          penPass::downstream_brownsmills$flow_cfs >= flow[7])),4],
        1
      ),
      
      # Howland
      runif(1, 0.978, 1), 
      
      # Mattaceunk
      ifelse(
        max(penPass::downstream_mattaceunk >= flow[7]),
        penPass::downstream_mattaceunk[min(which(
          penPass::downstream_mattaceunk$flow_cfs >= flow[7])),4],
        1
      ),
      
      # West Enfield
      ifelse(
        max(penPass::downstream_westenfield >= flow[7]),
        penPass::downstream_westenfield[min(which(
          penPass::downstream_westenfield$flow_cfs >= flow[7])),4],
        1
      ),
      
      # Milford
      ifelse(
        max(penPass::downstream_milford >= flow[7]),
        penPass::downstream_milford[min(which(
          penPass::downstream_milford$flow_cfs >= flow[7])),4],
        1
      ),
      
      # Great Works
      ifelse(
        max(penPass::downstream_greatworks >= flow[7]),
        penPass::downstream_greatworks[min(which(
          penPass::downstream_greatworks$flow_cfs >= flow[7])),4],
        1
      ),
      
      1, # Gilman Falls
      
      # Stillwater
      ifelse(
        max(penPass::downstream_stillwater >= flow[7]),
        penPass::downstream_stillwater[min(which(
          penPass::downstream_stillwater$flow_cfs >= flow[7])),4],
        1
      ),
      
      # Orono
      ifelse(
        max(penPass::downstream_orono >= flow[7]),
        penPass::downstream_orono[min(which(
          penPass::downstream_orono$flow_cfs >= flow[7])),4],
        1
      ),
      
      # Veazie
      ifelse(
        max(penPass::downstream_veazie >= flow[7]),
        penPass::downstream_veazie[min(which(
          penPass::downstream_veazie$flow_cfs >= flow[7])),4],
        1
      ),
      
      1 # Bangor
    )
      
  return(dam_survival)
  
}