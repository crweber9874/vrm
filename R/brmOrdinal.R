#' Fit a Bayesian Ordinal Regression Model
#'
#' A convenience wrapper around \code{\link{brms_fit}} for ordinal
#' (cumulative logit) regression.
#'
#' @inheritParams brms_fit
#' @param ... Additional arguments passed to \code{\link[brms]{brm}}.
#'
#' @return A \code{brmsfit} object.
#' @export
#'
#' @examples
#' \dontrun{
#' fit <- brms.ordinal(
#'   data      = df,
#'   DV        = "rating",
#'   treatment = "treatment",
#'   moderator = "group",
#'   controls  = "age + gender"
#' )
#' }
brms.ordinal <- function(data,
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
           family = cumulative(), weights = weights,
           chains = chains, iter = iter, warmup = warmup,
           cores = cores, seed = seed, ...)
}
