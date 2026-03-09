#' Posterior Predictive Difference (Post minus Pre)
#'
#' Computes the posterior distribution of the difference in expected
#' predictions between two levels of a treatment variable (e.g.,
#' post-election minus pre-election), conditional on each level of a
#' moderator (e.g., vote choice).
#'
#' For categorical / cumulative models, a specific response category can
#' be selected via \code{category}; the difference is then in predicted
#' probability for that category.
#'
#' @param model A \code{brmsfit} object.
#' @param xvar Character. Treatment variable whose levels are contrasted.
#' @param mvar Character. Moderator variable (results returned per level).
#' @param xval Numeric vector of length 2. The two levels of \code{xvar}
#'   to contrast. The difference is \code{xval[2] - xval[1]}.
#' @param mval Numeric vector. Levels of the moderator to evaluate.
#' @param category Character or \code{NULL}. For ordinal / multinomial
#'   models, filter to this response category before differencing.
#'   \code{NULL} keeps all categories (returns one row per category per
#'   moderator level).
#' @param covariates Named list of covariate overrides (see
#'   \code{\link{posterior_draws_grid}}).
#' @param ndraws Integer or \code{NULL}. Number of posterior draws.
#' @param prob Numeric. Width of the credible interval (default 0.95).
#' @return A \code{tibble} with columns: the moderator variable,
#'   (optionally) \code{.category}, \code{mean}, \code{lower},
#'   \code{upper}.
#'
#' @examples
#' \dontrun{
#' posterior_pred_diff(
#'   wss20_models[["recount_linear"]],
#'   xvar = "prepost", mvar = "vote_trump"
#' )
#' }
#'
#' @export
posterior_pred_diff <- function(model,
                               xvar  = "prepost",
                               mvar  = "vote_trump",
                               xval  = c(0, 1),
                               mval  = c(0, 1),
                               category   = NULL,
                               covariates = NULL,
                               ndraws     = NULL,
                               prob       = 0.95) {
  draws <- posterior_draws_grid(
    model, xvar = xvar, mvar = mvar,
    xval = xval, mval = mval,
    covariates = covariates, ndraws = ndraws
  )

  # Filter to a specific category if requested
  if (!is.null(category) && ".category" %in% names(draws)) {
    draws <- dplyr::filter(draws, .category == category)
  }

  alpha <- (1 - prob) / 2

  # Determine grouping columns
  has_cat <- ".category" %in% names(draws) && is.null(category)
  grp <- c(mvar, if (has_cat) ".category")

  # Split by treatment level, difference, summarise
  d_hi <- dplyr::filter(draws, .data[[xvar]] == xval[2])
  d_lo <- dplyr::filter(draws, .data[[xvar]] == xval[1])

  # Align on draw + moderator (+ category)
  join_by <- c(".draw", grp)
  diffs <- dplyr::inner_join(
    dplyr::select(d_hi, dplyr::all_of(c(join_by, ".epred"))),
    dplyr::select(d_lo, dplyr::all_of(c(join_by, ".epred"))),
    by = join_by, suffix = c("_hi", "_lo")
  ) %>%
    dplyr::mutate(diff = .epred_hi - .epred_lo) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::summarise(
      mean  = mean(diff),
      lower = stats::quantile(diff, alpha),
      upper = stats::quantile(diff, 1 - alpha),
      .groups = "drop"
    )

  diffs
}
