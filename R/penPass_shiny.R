#' @title Run the PenPass model interactively with shiny
#'
#' @description Run the PenPass model interactively in a browser using the shiny
#' package for R. Allows users to view or download summary data from multiple
#' runs of a single scenario.
#' 
#' @seealso run_one_year
#'
#' @examples 
#' # To open the shiny user-interface, run the following in R console:
#' # penPass_shiny()
#'
#' @export
penPass_shiny <- function() {
  appDir <- system.file("shiny-examples", "penPass_shiny", package = "penPass")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `penPass`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
