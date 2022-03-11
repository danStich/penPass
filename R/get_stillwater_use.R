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
#' 
#' Stich, DS, M Bailey, and JD Zydlewski. 2014. Survival of 
#' Atlantic Salmon Salmo salar smolts through a hydropower 
#' complex. Journal of Fish Biology 85:1074-1096.
#' 
#' @seealso stillwater_pars
#' 
#' @examples 
#' # Simulate probability of using Stillwater Branch
#' # based on randomly drawn flow at West Enfield Dam
#' flow_cfs <- rep(seq(0, 45000, 100), 5)
#' flow_cms <- flow_cfs * 0.028316847
#' use <- get_stillwater_use(flow_cfs)
#' plot(flow_cms, 
#'   use, 
#'   pch = 21, 
#'   bg = rgb(0,0,0,0.05),
#'   col = rgb(0,0,0,0.05)
#'   )
#' 
#' @export
#' 
get_stillwater_use <- function(flow){
  
  flow_cms <- flow * 0.028316847
  
  beta_draw <- stats::rnorm(length(flow), 
                     mean = penPass::stillwater_pars[2,2], 
                     sd = penPass::stillwater_pars[2,3]
                     ) 

  p_logit <- penPass::stillwater_pars[1, 2] + beta_draw * flow_cms
  p_stillwater <- exp(p_logit)/(1 + exp(p_logit))
  
  return(p_stillwater)
  
}