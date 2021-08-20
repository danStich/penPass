#' @title Simulate natural, per kilometer survival
#' 
#' @description Function used to simulate mortality per kilometer
#' from the empirical cumulative distribution function used by
#' Stevens et al. (2019).
#' 
#' @details A wrapper for the `sample()` function for resampling the empirical 
#' cumulative distribution function included in the \code{\link{mort_per_km}}
#' data set.
#' 
#' @examples
#' # Simulate 
#' morts <- sim_km_mort(
#'   prop_lost_per_km = penPass::mort_per_km$prop_lost_per_km,
#'   n = 1000,
#'   prob = penPass::mort_per_km$prob
#' )
#' 
#' # Calculate number surviving from 1000 starting individuals
#' surv <- 1e3 - (1e3 * exp(-morts)) 
#' hist( surv, breaks = 40 )
#' 
#' @export
#' 
sim_km_mort <- function(prop_lost_per_km, n, prob){
  
  km_mort <- sample(prop_lost_per_km, 
    size = n, 
    replace = TRUE,
    prob = prob)
  
  return(km_mort)
  
}

