#' @title cfa_model
#' @description Write the \pkg{lavaan} syntax for a single-factor (univariate)
#'   confirmatory factor model from a vector of indicator variables. The function
#'   only *builds and returns the model string* -- estimation (and the error
#'   screening that matters) is handled by [fit_cfa()].
#'
#' @param items Character vector of indicator (column) names.
#' @param factor Name of the latent factor. Default \code{"factor"}.
#'
#' @return A length-one character string of lavaan model syntax.
#' @seealso [fit_cfa()], [test_retest_model()]
#' @examples
#' cfa_model(c("x1", "x2", "x3"), factor = "ability")
#' @export
cfa_model <- function(items, factor = "factor") {
  if (!is.character(items) || length(items) < 2L)
    stop("`items` must be a character vector of at least two indicators.",
         call. = FALSE)
  paste(factor, "=~", paste(items, collapse = " + "))
}

#' @title test_retest_model
#' @description Write the \pkg{lavaan} syntax for a two-occasion latent
#'   test-retest model: one factor per occasion measured by the same item stems,
#'   a free factor covariance, and **no correlated measurement error**. The two
#'   occasions are generic measurement waves (T1, T2) -- they need not straddle
#'   any event. The function only *builds and returns the model string*;
#'   estimation and error screening are handled by [fit_test_retest()].
#'
#' @param items Character vector of item *stems* (without occasion suffix).
#' @param t1,t2 Occasion suffixes appended to each stem to form the observed
#'   column names \code{"<stem>_<t1>"} and \code{"<stem>_<t2>"}. Defaults
#'   \code{"t1"} and \code{"t2"}.
#' @param factor_t1,factor_t2 Names of the two latent factors.
#'
#' @return A length-one character string of lavaan model syntax.
#' @seealso [fit_test_retest()], [cfa_model()]
#' @examples
#' test_retest_model(c("a", "b", "c"))
#' @export
test_retest_model <- function(items, t1 = "t1", t2 = "t2",
                              factor_t1 = "factor_t1", factor_t2 = "factor_t2") {
  if (!is.character(items) || length(items) < 2L)
    stop("`items` must be a character vector of at least two item stems.",
         call. = FALSE)
  paste(
    paste(factor_t1, "=~", paste(paste0(items, "_", t1), collapse = " + ")),
    paste(factor_t2, "=~", paste(paste0(items, "_", t2), collapse = " + ")),
    paste(factor_t1, "~~", factor_t2),
    sep = "\n"
  )
}
