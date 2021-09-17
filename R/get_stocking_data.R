#' @title Get historical stocking data
#' 
#' @description Get number of smolts stocked at various stocking sites by year
#' in the Penobscot River, Maine, USA. Uses user-specified year to query the
#' `stocking_data` and merge with `distance_traveled` based on site_code.
#' 
#' @param year A numeric vector with a single year included in 
#' \code{\link{stocking_data}}.
#' 
#' @return A vector with dam passage rates 
#' 
#' @references
#' Stevens, JR, JF Kocik, and TF Sheehan. 2019. Modeling the impacts of dams and 
#' stocking practices on an endangered Atlantic salmon (Salmo salar) 
#' population in the Penobscot River, Maine, USA. Canadian Journal of Fisheries
#' and Aquatic Sciences 76:1795-1807.
#' 
#' @export
#' 
get_stocking_data <- function(year){
  
  stocks <- penPass::stocking_data[penPass::stocking_data$Year == year, ]
      
  stocked_smolts <- merge(penPass::distance_traveled, stocks, by = "site_code", all.x = TRUE)
  stocked_smolts$n_stocked[is.na(stocked_smolts$n_stocked)] <- 0
  
  stocked_smolts <- dplyr::select(stocked_smolts, c("site_code", "sub_basin",
                                                    "n_stocked"))
  
  return(stocked_smolts)
  
}