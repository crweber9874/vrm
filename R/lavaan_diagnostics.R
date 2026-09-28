# Internal: assemble the plain-language Heywood report.
.heywood_report <- function(heywood, negative_variances, out_of_bounds,
                            latent_not_pd, admissible) {
  if (!heywood)
    return("No Heywood case detected: the lavaan solution is admissible.")

  found <- character(0)
  if (length(negative_variances))
    found <- c(found, sprintf(
      "  - Negative estimated variance(s): %s. The residual (unique) variance is below zero, i.e. the factor is credited with explaining 'more than 100%%' of the item.",
      paste(sprintf("%s = %.3f", names(negative_variances), negative_variances),
            collapse = "; ")))
  if (!is.null(out_of_bounds) && nrow(out_of_bounds))
    found <- c(found, sprintf(
      "  - Standardized estimate(s) outside [-1, 1]: %s. A standardized loading > 1 implies a negative residual variance; a latent correlation at/above 1 implies two factors are empirically indistinguishable.",
      paste(sprintf("%s %s %s = %.3f", out_of_bounds$lhs, out_of_bounds$op,
                    out_of_bounds$rhs, out_of_bounds$est.std), collapse = "; ")))
  if (latent_not_pd)
    found <- c(found,
      "  - The covariance matrix of the latent variables is not positive definite.")
  if (!admissible && !length(found))
    found <- c(found,
      "  - lavaan's post-fit admissibility check failed (lavInspect(fit, 'post.check') is FALSE).")

  paste0(
    "HEYWOOD CASE DETECTED -- the solution is improper (inadmissible); do not interpret the estimates.\n\n",
    "A Heywood case is a parameter estimate that lies outside its logical bounds: a NEGATIVE variance, ",
    "or a standardized loading/correlation with absolute value greater than 1 (equivalently, a covariance ",
    "matrix of the latent variables that is not positive definite).\n\n",
    "What was found:\n",
    paste(found, collapse = "\n"),
    "\n\nWhat to check:\n",
    "  - Negative residual variances: inspect lavInspect(fit, 'theta'); an item may be near-collinear with the factor, have an outlier, or a sparse response category.\n",
    "  - Latent correlations at/above 1: the factors are collinear -- consider merging them or fitting a single factor.\n",
    "  - Misspecification: the indicators may be multidimensional, or the wrong number of factors is imposed.\n",
    "  - Too few indicators per factor (< 3) or empirical under-identification.\n",
    "  - Small sample, sparse ordinal categories, or outliers (especially with ordered/WLSMV estimation).\n",
    "  - Near-linear dependencies / multicollinearity among the indicators."
  )
}

#' @title check_heywood
#' @description Inspect a fitted \pkg{lavaan} model for **Heywood cases** /
#'   improper (inadmissible) solutions and report, in plain language, what was
#'   found and what to check. A Heywood case is a parameter estimate that lies
#'   outside its logical bounds: a negative estimated variance, or a standardized
#'   loading/correlation with absolute value greater than 1 (equivalently, a
#'   covariance matrix of the latent variables that is not positive definite).
#'   Such a solution is improper and its estimates should not be interpreted.
#'
#' @param fit A fitted object of class \code{lavaan}.
#' @param tol Numeric tolerance for flagging negative variances and
#'   out-of-bounds standardized estimates. Default \code{1e-4}.
#' @param warn Logical; if \code{TRUE} (default) a single \code{warning()}
#'   carrying the diagnosis and guidance is signalled when a Heywood case is
#'   detected.
#'
#' @return Invisibly, a list with elements \code{heywood} (logical),
#'   \code{negative_variances} (named numeric), \code{out_of_bounds} (data frame
#'   of standardized estimates whose magnitude exceeds 1), \code{latent_not_pd}
#'   (logical), \code{admissible} (lavaan's post-fit check), and \code{message}
#'   (the formatted report).
#'
#' @seealso [fit_cfa()], [fit_test_retest()]
#' @export
check_heywood <- function(fit, tol = 1e-4, warn = TRUE) {
  if (!requireNamespace("lavaan", quietly = TRUE))
    stop("Package 'lavaan' is required for check_heywood().", call. = FALSE)
  if (!inherits(fit, "lavaan"))
    stop("`fit` must be a fitted lavaan object.", call. = FALSE)

  safe_diag <- function(m) if (is.matrix(m)) diag(m) else if (is.numeric(m)) m else numeric(0)

  # negative variances: residual (theta) + latent (psi)
  theta <- tryCatch(safe_diag(lavaan::lavInspect(fit, "theta")),
                    error = function(e) numeric(0))
  psi   <- tryCatch(safe_diag(lavaan::lavInspect(fit, "psi")),
                    error = function(e) numeric(0))
  vars  <- c(theta, psi)
  negative_variances <- vars[is.finite(vars) & vars < -tol]

  # standardized loadings / correlations with |est| > 1
  std <- tryCatch(lavaan::standardizedSolution(fit), error = function(e) NULL)
  out_of_bounds <- if (is.null(std)) NULL else
    std[std$op %in% c("=~", "~~") & std$lhs != std$rhs &
          is.finite(std$est.std) & abs(std$est.std) > 1 + tol, , drop = FALSE]

  # latent covariance not positive definite
  cov_lv <- tryCatch(lavaan::lavInspect(fit, "cov.lv"), error = function(e) NULL)
  latent_not_pd <- FALSE
  if (is.matrix(cov_lv) && nrow(cov_lv) > 0L) {
    ev <- eigen(cov_lv, symmetric = TRUE, only.values = TRUE)$values
    latent_not_pd <- any(ev < -tol)
  }

  admissible <- isTRUE(suppressWarnings(lavaan::lavInspect(fit, "post.check")))

  heywood <- length(negative_variances) > 0L ||
    (!is.null(out_of_bounds) && nrow(out_of_bounds) > 0L) ||
    latent_not_pd || !admissible

  msg <- .heywood_report(heywood, negative_variances, out_of_bounds,
                         latent_not_pd, admissible)
  if (heywood && warn) warning(msg, call. = FALSE)

  invisible(list(
    heywood = heywood,
    negative_variances = negative_variances,
    out_of_bounds = out_of_bounds,
    latent_not_pd = latent_not_pd,
    admissible = admissible,
    message = msg
  ))
}

# Internal: fit a lavaan measurement model with error caching + Heywood screen.
.fit_measurement <- function(model, data, indicators, ordered, missing,
                             std.lv, warn_heywood, ...) {
  if (!requireNamespace("lavaan", quietly = TRUE))
    stop("Package 'lavaan' is required to fit measurement models.", call. = FALSE)

  miss_cols <- setdiff(indicators, names(data))
  if (length(miss_cols))
    stop("Indicator column(s) not found in `data`: ",
         paste(miss_cols, collapse = ", "), call. = FALSE)

  ord <- if (isTRUE(ordered)) indicators
         else if (isFALSE(ordered)) NULL
         else ordered

  fit <- tryCatch(
    lavaan::cfa(model = model, data = data, ordered = ord,
                missing = missing, std.lv = std.lv, ...),
    error = function(e) stop(
      "lavaan could not fit the model: ", conditionMessage(e),
      "\nCheck that every indicator exists in `data`, has variance (no constant ",
      "or empty columns), and that the sample is large enough for the requested ",
      "estimator (ordinal/WLSMV needs adequate counts in every response category).",
      call. = FALSE)
  )

  attr(fit, "heywood") <- check_heywood(fit, warn = warn_heywood)
  fit
}

#' @title fit_cfa
#' @description Build (via [cfa_model()]) and fit a single-factor confirmatory
#'   factor model, then screen the solution for Heywood cases with
#'   [check_heywood()]. Estimation errors are caught and re-raised with an
#'   actionable message; an improper solution triggers a clear warning rather
#'   than passing silently.
#'
#' @param data A data frame containing the indicator columns.
#' @param items Character vector of indicator (column) names.
#' @param factor Name of the latent factor. Default \code{"factor"}.
#' @param ordered Treat indicators as ordinal? \code{TRUE} (default) declares all
#'   \code{items} ordered (WLSMV); pass a character vector to mark a subset, or
#'   \code{FALSE} for continuous (ML).
#' @param missing lavaan missing-data handling. Default \code{"listwise"}.
#' @param std.lv Fix latent variances to 1? Default \code{TRUE}.
#' @param warn_heywood Emit a warning when a Heywood case is detected? Default
#'   \code{TRUE}.
#' @param ... Additional arguments passed to \code{lavaan::cfa}.
#'
#' @return The fitted \code{lavaan} object, with the [check_heywood()] result
#'   stored in attribute \code{"heywood"}.
#' @seealso [cfa_model()], [check_heywood()], [fit_test_retest()]
#' @export
fit_cfa <- function(data, items, factor = "factor",
                    ordered = TRUE, missing = "listwise", std.lv = TRUE,
                    warn_heywood = TRUE, ...) {
  .fit_measurement(cfa_model(items, factor), data, items,
                   ordered, missing, std.lv, warn_heywood, ...)
}

#' @title fit_test_retest
#' @description Build (via [test_retest_model()]) and fit a two-occasion latent
#'   test-retest model (no correlated measurement error), then screen the
#'   solution for Heywood cases with [check_heywood()]. Each item stem must
#'   appear in \code{data} suffixed by \code{_<t1>} and \code{_<t2>}; a missing
#'   suffix raises an informative error.
#'
#' @param data A data frame in wide form with columns \code{"<stem>_<t1>"} and
#'   \code{"<stem>_<t2>"} for every stem in \code{items}.
#' @param items Character vector of item *stems* (without occasion suffix).
#' @param t1,t2 Occasion suffixes. Defaults \code{"t1"}, \code{"t2"}.
#' @param factor_t1,factor_t2 Names of the two latent factors.
#' @param ordered,missing,std.lv,warn_heywood,... As in [fit_cfa()].
#'
#' @return The fitted \code{lavaan} object, with the [check_heywood()] result
#'   stored in attribute \code{"heywood"}.
#' @seealso [test_retest_model()], [check_heywood()], [fit_cfa()]
#' @export
fit_test_retest <- function(data, items, t1 = "t1", t2 = "t2",
                            factor_t1 = "factor_t1", factor_t2 = "factor_t2",
                            ordered = TRUE, missing = "listwise", std.lv = TRUE,
                            warn_heywood = TRUE, ...) {
  indicators <- c(paste0(items, "_", t1), paste0(items, "_", t2))
  miss <- setdiff(indicators, names(data))
  if (length(miss))
    stop("Each item stem must be suffixed by _", t1, " and _", t2,
         ". Missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
  model <- test_retest_model(items, t1, t2, factor_t1, factor_t2)
  .fit_measurement(model, data, indicators, ordered, missing, std.lv,
                   warn_heywood, ...)
}
