# Family-specific convenience wrappers around brms_fit().
#
# These previously lived in four separate files (brmOrdinal.R, brmsLinear.R,
# brmsLogit.R, brmsNominal.R) whose bodies were identical apart from `family`.

#' Fit a brms model with a fixed response family
#'
#' Thin wrappers around [brms_fit()] that pin the response family:
#' `brms.linear()` (gaussian), `brms.binary()` (bernoulli),
#' `brms.ordinal()` (cumulative), and `brms.nominal()` (categorical).
#'
#' @param data A data frame.
#' @param DV Character. Name of the outcome variable.
#' @param treatment,moderator,controls,IV Character vectors of predictor names
#'   passed through to [build_formula_rhs()].
#' @param weights Optional name of a survey-weight column.
#' @param chains,iter,warmup,cores,seed Sampler settings passed to [brms::brm()].
#' @param ... Further arguments passed to [brms_fit()].
#' @return A `brmsfit` object.
#' @name brms_families
NULL

#' @rdname brms_families
#' @export
brms.linear <- function(data, DV, treatment = NULL, moderator = NULL,
                        controls = NULL, IV = NULL, weights = NULL,
                        chains = 4, iter = 2000, warmup = 1000,
                        cores = 4, seed = 1234, ...) {
  brms_fit(data = data, DV = DV, treatment = treatment, moderator = moderator,
           controls = controls, IV = IV, family = stats::gaussian(),
           weights = weights, chains = chains, iter = iter, warmup = warmup,
           cores = cores, seed = seed, ...)
}

#' @rdname brms_families
#' @export
brms.binary <- function(data, DV, treatment = NULL, moderator = NULL,
                        controls = NULL, IV = NULL, weights = NULL,
                        chains = 4, iter = 2000, warmup = 1000,
                        cores = 4, seed = 1234, ...) {
  brms_fit(data = data, DV = DV, treatment = treatment, moderator = moderator,
           controls = controls, IV = IV, family = brms::bernoulli(),
           weights = weights, chains = chains, iter = iter, warmup = warmup,
           cores = cores, seed = seed, ...)
}

#' @rdname brms_families
#' @export
brms.ordinal <- function(data, DV, treatment = NULL, moderator = NULL,
                         controls = NULL, IV = NULL, weights = NULL,
                         chains = 4, iter = 2000, warmup = 1000,
                         cores = 4, seed = 1234, ...) {
  brms_fit(data = data, DV = DV, treatment = treatment, moderator = moderator,
           controls = controls, IV = IV, family = brms::cumulative(),
           weights = weights, chains = chains, iter = iter, warmup = warmup,
           cores = cores, seed = seed, ...)
}

#' @rdname brms_families
#' @export
brms.nominal <- function(data, DV, treatment = NULL, moderator = NULL,
                         controls = NULL, IV = NULL, weights = NULL,
                         chains = 4, iter = 2000, warmup = 1000,
                         cores = 4, seed = 1234, ...) {
  brms_fit(data = data, DV = DV, treatment = treatment, moderator = moderator,
           controls = controls, IV = IV, family = brms::categorical(),
           weights = weights, chains = chains, iter = iter, warmup = warmup,
           cores = cores, seed = seed, ...)
}
