#' @title Get annual flows
#' 
#' @description Simulate correlated annual flows at USGS gage stations 
#' in the Penobscot River watershed based on cumulative probability 
#' distribution used by Stevens et al. (2019).
#' 
#' @param year Year of simulation
#' 
#' @return A vector with discharge estimates drawn from a cdf 
#' 
#' @references
#' Stevens, JR, JF Kocik, and TF Sheehan. 2019. Modeling the impacts of dams and 
#' stocking practices on an endangered Atlantic salmon (Salmo salar) 
#' population in the Penobscot River, Maine, USA. Canadian Journal of Fisheries
#' and Aquatic Sciences 76:1795-1807.
#' @export
#' 
get_annual_flows <- function(year = 2020){
  
    # Flow for year
    flows <- penPass::flow_ranks[penPass::flow_ranks$Year == year, ]
  
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
    
    annual_flows <- round(annual_flows, -1.5)
    
    stillwater_prop <- 0
    
    if(annual_flows[14] < min(penPass::stillwater_splits$total_flow)){
      stillwater_prop <- min(penPass::stillwater_splits$prop_stillwater)
    }
    if(annual_flows[14] > max(penPass::stillwater_splits$total_flow)){
      stillwater_prop <- max(penPass::stillwater_splits$prop_stillwater)
    }
    
    if(annual_flows[14] >= min(penPass::stillwater_splits$total_flow) & 
       annual_flows[14] <= min(penPass::stillwater_splits$total_flow)){
      
      stillwater_prop <- penPass::stillwater_splits$prop_stillwater[
        penPass::stillwater_splits$total_flow == annual_flows[14]
      ]
      
    }
    
    annual_flows[15:16] <- round(
      annual_flows[15:16] * (1 - stillwater_prop), -1.5)
    
    annual_flows[17:19] <- round(annual_flows[17:19] * stillwater_prop, -1.5)
    
  return(annual_flows)
  
}