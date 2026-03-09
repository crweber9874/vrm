#' Run Shiny App for Ordered Regression Models
#'
#' Launches an interactive Shiny application for exploring ordered logit and probit models.
#'
#' @param ... Additional arguments passed to shiny::runApp()
#'
#' @return No return value, launches Shiny app
#' @export
#'
#' @examples
#' \dontrun{
#' run_shiny_app()
#' }
run_shiny_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Please install it with: install.packages('shiny')")
  }
  
  app_dir <- system.file("shiny-examples", "ordinal_models", package = "weberUtilties")
  
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Try re-installing the package.")
  }
  
  shiny::runApp(app_dir, ...)
}
