
#' Stan binary Model
#'
#' @param Specify DV, IV, data, and other arguments native to BRMS. DV must be binary, consisting of two levels.
#'
#' @return brms model object, binary regression (Logit)
#' @export
#'
#'
brms.binary <- function(data = dataActive, IV = IV, DV = DV ,  chains = 4, iter = 2000, warmup = 1000, cores = 10, seed = 1234,...){


        modelFormula <- stats::as.formula(paste(DV, "~", IV))
        # Fit the model
        model <- brms::brm(
          formula = modelFormula,
          data = data,
         family = bernoulli(),
          ...
        )
  return(model)
}


