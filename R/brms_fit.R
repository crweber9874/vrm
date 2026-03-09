#' Fit a brms regression model
#'
#' A convenience wrapper around \code{\link[brms]{brm}} that accepts
#' dependent and independent variables as character strings and builds
#' the formula automatically. Supports any brms family.
#'
#' @param data A data.frame. Required -- no default.
#' @param DV Character string naming the dependent variable.
#' @param IV Character string or formula fragment for independent variable(s)
#'   (e.g., \code{"x1 + x2"} or \code{"x1 * x2 + x3"}).
#' @param family A \pkg{brms} family object. Defaults to \code{gaussian()}.
#'   Common choices: \code{cumulative("logit")}, \code{bernoulli()},
#'   \code{categorical()}, \code{gaussian()}.
#' @param chains Number of MCMC chains. Default 4.
#' @param iter Total iterations per chain. Default 2000.
#' @param warmup Warmup iterations. Default 1000.
#' @param cores Number of cores. Default 4.
#' @param seed Random seed. Default 1234.
#' @param ... Additional arguments passed to \code{\link[brms]{brm}}.
#'
#' @return A \code{brmsfit} object.
#' @export
#'
#' @examples
#' \dontrun{
#' # Linear model
#' m <- brms_fit(mtcars, "mpg", "hp + wt")
#'
#' # Ordinal model
#' m <- brms_fit(df, "rating", "treatment * group", family = cumulative("logit"))
#'
#' # Binary logistic model
#' m <- brms_fit(df, "outcome", "x1 + x2", family = bernoulli())
#' }
brms_fit <- function(data, DV, IV, family = gaussian(),
                     chains = 4, iter = 2000, warmup = 1000,
                     cores = 4, seed = 1234, ...) {
  model_formula <- stats::as.formula(paste(DV, "~", IV))
  brms::brm(
    formula = model_formula,
    data = data,
    family = family,
    chains = chains,
    iter = iter,
    warmup = warmup,
    cores = cores,
    seed = seed,
    ...
  )
}
