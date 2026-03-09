
#' Stan Linear Model
#'
#' @param Specify DV, IV, data, and other arguments native to BRMS
#'
#' @return brms model object
#' @export
#'
#'
brms.linear <- function(data = dataActive, IV = IV, DV = DV,  chains = 4, iter = 2000, warmup = 1000, cores = 10, seed = 1234,...){

    # Create a model formula
        modelFormula <- stats::as.formula(paste(DV, "~", IV))
        # Fit the model
        model <- brms::brm(
          formula = modelFormula,
          data = data,
          ...
        )
  return(model)
}


