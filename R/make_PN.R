#' @title Create Mainstem Penobscot River smolt population
#' 
#' @description Function used to create a dataframe containing river segments
#' and sites for stocking and migration as well as dams from built-in data sets,
#' user-specified arguments in \code{\link{run_one_year}}, and output of
#' \code{\link{get_stocking_data}} for a given `year` in the Penobscot
#' River from Stevens et al. (2019).
#' 
#' @param stocking A dataframe matching output of 
#' \code{link{get_stocking_data}}.
#' 
#' @param prod A named vector matching the `prod` argument specified in 
#' \code{\link{run_one_year}}
#' 
#' @param sat A named vector matching the `sat` argument specified in 
#' \code{\link{run_one_year}}
#' 
#' @references 
#' Stevens, JR, JF Kocik, and TF Sheehan. 2019. Modeling the impacts of dams and 
#' stocking practices on an endangered Atlantic salmon (Salmo salar) 
#' population in the Penobscot River, Maine, USA. Canadian Journal of Fisheries
#' and Aquatic Sciences 76:1795-1807.
#' 
#' @export
#' 
make_PN <- function(stocking, prod, sat){
  
  pn <- penPass::PN
  
  pn$saturation[!is.na(pn$saturation)] <- sat["PN_sat"]
  
  pn$n_smolts <- pn$available_habitat_units_or_segment_length *
    prod["PN_prod"] * pn$saturation
  
  # Stocking
  pn$n_smolts[11] <- stocking$n_stocked[27]
  pn$n_smolts[12] <- stocking$n_stocked[26]
  pn$n_smolts[17] <- stocking$n_stocked[25]
  pn$n_smolts[18] <- stocking$n_stocked[24]
  pn$n_smolts[19] <- stocking$n_stocked[23]
  pn$n_smolts[28] <- stocking$n_stocked[22]
  pn$n_smolts[29] <- stocking$n_stocked[42]
  pn$n_smolts[32] <- stocking$n_stocked[21]
  pn$n_smolts[37] <- stocking$n_stocked[20]
  pn$n_smolts[38] <- stocking$n_stocked[19]
  pn$n_smolts[42] <- stocking$n_stocked[41]
  pn$n_smolts[45] <- stocking$n_stocked[43]
  pn$n_smolts[46] <- stocking$n_stocked[40]
  pn$n_smolts[55] <- stocking$n_stocked[38]
  pn$n_smolts[58] <- stocking$n_stocked[37]
  pn$n_smolts[64] <- stocking$n_stocked[18]
  pn$n_smolts[67] <- stocking$n_stocked[36]
  pn$n_smolts[72] <- stocking$n_stocked[35]
  pn$n_smolts[75] <- stocking$n_stocked[34]
  pn$n_smolts[96] <- stocking$n_stocked[33]
  pn$n_smolts[97] <- stocking$n_stocked[32]
  pn$n_smolts[100] <- stocking$n_stocked[31]
  pn$n_smolts[103] <- stocking$n_stocked[30]
  pn$n_smolts[107] <- stocking$n_stocked[29]
  pn$n_smolts[108] <- stocking$n_stocked[28]
  pn$n_smolts[121] <- stocking$n_stocked[39]

  return(pn)
  
}

