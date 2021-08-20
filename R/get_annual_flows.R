#' @title Get dam-specific survival rates
#' 
#' @description Get correlated dam-specific downstream survival rates for 
#' smolts at each dam from built-in `downstream_` datasets or default values
#' based on triangular distribution. Values based on Amaral et al. (2012) and 
#' sampled following Nieland et al. (2013, 2015) and Nieland and Sheehan (2020),
#' or sampled using `EnvStats::rtri()` for other dams based on Stevens et al. 
#' (2019).
#' 
#' @param new_or_old A character string indicating whether to use `"new"` 
#' (Amaral et al. 2012, Nieland and Sheehan 2020) or `"old"` (Amaral et al. 2012, 
#' Nieland et al. 2020) flow-correlated probabilities of p_stillwater as well 
#' as flow-correlated survival at `milford`, `orono`, and `stillwater` dams.
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
get_annual_flows <- function(year = 2020){
  
    # Flow for year
    flows <- penPass::flow_ranks %>% 
      filter(Year == year)
  
    # Quantile for iteration/year
    quant <- sample(unique(flows$cum_prob), 1, replace = TRUE)
    
    # Single samples for shared gages
    samp_01031500 <- sample(flows[flows$GageID == 01031500, 6], 1)
    samp_01034500 <- sample(flows[flows$GageID == 01034500, 6], 1)
    
    
    # Flows by dam based on Gage ID in Parameters worksheet
    annual_flows <- c(
      rep(NA, 6), # Upper river dams use rtri later with no flow
      sample(flows[flows$GageID == 01028000, 6], 1), # Medway
      NA, # Mattagamon
      NA, # Guilford
      samp_01031500, # Moosehead
      samp_01031500, # BrownsMill
      sample(flows[flows$GageID == 01034000, 6], 1), # Howland
      sample(flows[flows$GageID == 01030000, 6], 1), # Mattaceunk
      samp_01034500, # West Enfield
      samp_01034500, # Milford
      samp_01034500, # GreatWorks
      NA, # Gilman
      samp_01034500, # Stillwater
      samp_01034500, # Orono
      samp_01034500, # Veazie
      NA  # Bangor
    )
    
    # annual_flows_adj <- round(annual_flows * penPass::flow_ratios$flow_ratio, 0)
    
      
  return(annual_flows)
  
}