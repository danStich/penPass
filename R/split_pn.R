#' @title Split lower Penobscot River smolts
#' 
#' @description Function used to split migrating smolt population
#' into migratory routes and add number of dams by migration
#' route through the Penobscot River based on starting
#' sub_basin in \code{\link{distance_traveled}}.
#' 
#' @param basin_mat A dataframe matching \code{link{make_WPN_hazards}}, 
#'  \code{link{make_EPN_hazards}},  \code{link{make_Matt_hazards}},
#'  \code{link{make_PISC_hazards}}, or \code{link{make_PN_hazards}}.
#' 
#' @param river_mat A dataframe matching \code{link{make_PN_hazards}}.
#' 
#' @param p_stillwater Probability of using the Stillwater Branch for
#' migration around Marsh Island.
#' 
#' @references 
#' Stevens, JR, JF Kocik, and TF Sheehan. 2019. Modeling the impacts of dams and 
#' stocking practices on an endangered Atlantic salmon (Salmo salar) 
#' population in the Penobscot River, Maine, USA. Canadian Journal of Fisheries
#' and Aquatic Sciences 76:1795-1807.
#' 
#' @export
#' 
split_pn <- function(basin_mat, river_mat, p_stillwater){
  
  # Collection unit/segments in lower PN that receive inputs from
  # upper watershed sub_basin(s)
  confs <- grep(pattern = "8: ", river_mat$huc_collection_segment_or_damname)
  
  # Dam counts by upper river sub_basin
  conf_dams <- c(
    length(grep(pattern = "Dam", basin_mat$huc_collection_segment_or_damname)))
  
  # Collection unit/segment indices that contain dams within the mainstem 
  # Penobscot River
  dams <- grep(pattern = "Dam", river_mat$huc_collection_segment_or_damname)
  
  # Stillwater fish assignment ----
  # Copy river_mat to split it
  river_mat_s <- river_mat
  
  # Proportion of fish that use Stillwater Branch
  river_mat_s$n_smolts[1:61] <- 
    round(river_mat$n_smolts[1:61] * p_stillwater, 0)
  
  # No fish in mainstem PUs
  river_mat_s$n_smolts[62:75] <- 0
  
  # Can't have mortality through the mainstem for these fish!
  river_mat_s$hazard[62:75] <- 1  
  river_mat_s$n_smolts[91:125] <- 0 # Fish below confluence, will add back
  
  # Get dams in migration route (assign a 1 to each)
  river_mat_s$n_dams <- 0
  river_mat_s$n_dams[dams[!(dams %in% 62:75)]] <- 1
  
  # Get confluence dams separately. Will iterate over these in 
  # downstream migration
  river_mat_s$conf_dams <- 0
  river_mat_s$conf_dams[confs] <- conf_dams

  
  # Mainstem fish assignment ----
  # Copy river_mat to split it
  river_mat_m <- river_mat
  
  # Proportion of fish that use Stillwater Branch
  river_mat_m$n_smolts[1:61] <- 
    round(river_mat$n_smolts[1:61] * (1 - p_stillwater), 0)
  
  # No fish in mainstem PUs
  river_mat_m$n_smolts[76:90] <- 0
  # Can't have mortality through the Stillwater Branch for these fish!
  river_mat_m$hazard[76:90] <- 1  
  
  # Get dams in migration route (assign a 1 to each)
  river_mat_m$n_dams <- 0
  river_mat_m$n_dams[dams[!(dams %in% 76:90)]] <- 1
  
  # Get confluence dams separately. Will iterate over these in 
  # downstream migration
  river_mat_m$conf_dams <- 0
  river_mat_m$conf_dams[confs] <- conf_dams

  # Return the Stillwater and mainstem migrants
  return(list(
    river_mat_s = river_mat_s, 
    river_mat_m = river_mat_m
    ))
    
}

