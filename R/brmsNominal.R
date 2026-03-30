#' Fit a Bayesian Nominal (Multinomial) Regression Model
#'
#' A convenience wrapper around \code{\link{brms_fit}} for nominal
#' (categorical/multinomial) regression.
#'
#' @inheritParams brms_fit
#' @param ... Additional arguments passed to \code{\link[brms]{brm}}.
#'
#' @return A \code{brmsfit} object.
#' @export
#'
#' @examples
#' \dontrun{
#' fit <- brms.nominal(
#'   data      = df,
#'   DV        = "party",
#'   treatment = "income",
#'   controls  = "age + education"
#' )
#' }
brms.nominal <- function(data,
                         DV,
                         treatment = NULL,
                         moderator = NULL,
                         controls  = NULL,
                         IV        = NULL,
                         weights   = NULL,
                         chains = 4, iter = 2000, warmup = 1000,
                         cores = 4, seed = 1234, ...) {
  brms_fit(data = data, DV = DV,
           treatment = treatment, moderator = moderator,
           controls = controls, IV = IV,
           family = categorical(), weights = weights,
           chains = chains, iter = iter, warmup = warmup,
           cores = cores, seed = seed, ...)
}
