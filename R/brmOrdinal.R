#' @title brms.ordinal
#' @description Estimate an ordinal regression model in `brms`
#'
#' @param Specify DV, IV, data, and other arguments native to BRMS.
#' For example, with +1 IV and/or two way interactions;
#'  IV <- "presvote_trump_2020 + prepost + presvote_trump_2020:prepost"
#'
#' @return brms model object
#' @export
#
brms.ordinal <- function(data = dataActive, IV = IV, DV = DV,
                         chains = 4, iter = 2000, warmup = 1000,
                         cores = 10, seed = 1234, ...){

  # Create a model formula
  modelFormula <- stats::as.formula(paste(DV, "~", IV))
  # Fit the model
  model <- brms::brm(
    formula = modelFormula,
    data = data,
    family = cumulative(),
    ...
  )
  return(model)
}


