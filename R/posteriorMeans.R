#' Generate Posterior Predicted Means with Two-Way Interactions
#'
#' Computes posterior predicted means from a \pkg{brms} model across a grid
#' of two interacting variables, holding all other covariates at their means.
#' Supports ordinal, multinomial, binary, and Gaussian families.
#'
#' @param model A \code{brmsfit} object.
#' @param xvar Character. Name of the primary independent variable.
#' @param mvar Character. Name of the moderator variable.
#' @param xval Numeric vector. Values of \code{xvar} to predict at.
#'   Default: \code{seq(0, 1)}.
#' @param mval Numeric vector. Values of \code{mvar} to predict at.
#'   Default: \code{c(0, 1)}.
#'
#' @return A tibble with posterior predicted means and 95\% credible intervals
#'   for each combination of \code{xvar} and \code{mvar}.
#' @export
#'
#' @examples
#' \dontrun{
#' preds <- posterior_means(fit,
#'   xvar = "age", mvar = "treatment",
#'   xval = c(0, 0.5, 1), mval = c(0, 1)
#' )
#' }
posterior_means <- function(
  model,
  xvar = "presvote_trump_2020",
  mvar = "prepost",
  xval = seq(0, 1),
  mval = c(0, 1)
) {
  formula <- model$formula
  data <- model$data
  cols_to_average <- setdiff(names(data)[-1], c(xvar, mvar))
  data_grid <- data %>%
    dplyr::select(dplyr::all_of(cols_to_average)) %>%
    dplyr::summarize(dplyr::across(dplyr::everything(), mean)) %>%
    tidyr::expand_grid(
      !!xvar := xval,
      !!mvar := mval
    ) %>%
    tidybayes::add_epred_draws(model)
  if (model$family[[1]] == "categorical" |
    model$family[[1]] == "cumulative") {
    plot <- data_grid %>%
      dplyr::group_by(!!rlang::sym(xvar), !!rlang::sym(mvar), .category) %>%
      dplyr::summarize(
        mean = mean(.epred),
        lower = stats::quantile(.epred, 0.025),
        upper = stats::quantile(.epred, 0.975)
      )
  }
  if (model$family[[1]] == "bernoulli" |
    model$family[[1]] == "gaussian") {
    plot <- data_grid %>%
      dplyr::group_by(!!rlang::sym(xvar), !!rlang::sym(mvar)) %>%
      dplyr::summarize(
        mean = mean(.epred),
        lower = stats::quantile(.epred, 0.025),
        upper = stats::quantile(.epred, 0.975)
      )
  }
  return(plot)
}
