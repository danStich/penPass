#' @title Get probability of fish using Stillwater Branch
#' 
#' @description Get flow-conditional probability of fish using the Stillwater
#' Branch during downstream migration around Marsh Island in the lower Penobscot
#' River.
#' 
#' @param flow A numeric vector containing flow (cfs) at USGS gage 01034500
#' 
#' @return A numeric vector with probability that fish use Stillwater Branch 
#' 
#' @references
#' Nieland JL, Sheehan TF. 2020. Quantifying the Effects of Dams on Atlantic Salmon
#' in the Penobscot River Watershed, with a Focus on Weldon Dam. US Department of 
#' Commerce, Northeast Fisheries Science Center Reference Document 19-16, Woods 
#' Hole, MA.
#' 
#' Nieland JL, Sheehan TF, Saunders R. 2015. Assessing demographic effects of dams
#' on diadromous fish: a case study for Atlantic salmon in the Penobscot River, 
#' Maine. ICES Journal of Marine Science 72:2423–2437. 
#' 
#' Stevens, JR, JF Kocik, and TF Sheehan. 2019. Modeling the impacts of dams and 
#' stocking practices on an endangered Atlantic salmon (Salmo salar) 
#' population in the Penobscot River, Maine, USA. Canadian Journal of Fisheries
#' and Aquatic Sciences 76:1795-1807.
#' @export
#' 
get_stillwater_use <- function(flow){
  
  flow_cms <- flow * 0.028316847
  
  beta_draw <- stats::rnorm(1, 
                     mean = penPass::stillwater_pars[2,2], 
                     sd = penPass::stillwater_pars[2,3]
                     ) 

  p_logit <- penPass::stillwater_pars[1, 2] + beta_draw * flow_cms
  p_stillwater <- exp(p_logit)/(1 + exp(p_logit))
  
  return(p_stillwater)
  
}