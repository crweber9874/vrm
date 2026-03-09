#' Extract Posterior Prediction Draws Across a Covariate Grid
#'
#' Returns raw posterior draws from a \code{brmsfit} model evaluated over a
#' grid of treatment and moderator values.
#' All other covariates are held at specified values (column means by default).
#' The output is suitable for ridgeplots, density plots, or any visualisation
#' that requires the full posterior distribution rather than point summaries.
#'
#' @param model A \code{brmsfit} object.
#' @param xvar Character. Name of the treatment / main predictor variable.
#' @param mvar Character. Name of the moderator variable.
#' @param xval Numeric vector. Values of \code{xvar} to predict over.
#' @param mval Numeric vector. Values of \code{mvar} to predict over.
#' @param covariates Named list of covariate overrides.
#'   Any covariate not listed is held at its observed column mean.
#'   Use this to set specific covariate profiles
#'   (e.g., \code{list(female = 1, college = 0)}).
#' @param ndraws Integer or \code{NULL}. Number of posterior draws to retain.
#'   \code{NULL} (default) keeps all draws.
#' @return A \code{tibble} with one row per draw per grid cell, containing
#'   the grid variables, \code{.draw}, \code{.epred}, and---for categorical
#'   or cumulative families---\code{.category}.
#'
#' @details
#' The function builds a prediction grid by crossing \code{xval} and
#' \code{mval}, then calls \code{tidybayes::add_epred_draws()} to obtain
#' posterior expected predictions.
#'
#' For \strong{gaussian} models, \code{.epred} is the predicted mean.
#' For \strong{cumulative / categorical} models, \code{.epred} is the
#' predicted probability for each response category stored in
#' \code{.category}.
#'
#' @examples
#' \dontrun{
#' draws <- posterior_draws_grid(
#'   model = wss20_models[["recount_linear"]],
#'   xvar  = "prepost",
#'   mvar  = "vote_trump",
#'   xval  = c(0, 1),
#'   mval  = c(0, 1)
#' )
#'
#' # With covariate overrides
#' draws <- posterior_draws_grid(
#'   model      = wss20_models[["recount_ord"]],
#'   xvar       = "prepost",
#'   mvar       = "vote_trump",
#'   covariates = list(female = 1, college = 1),
#'   ndraws     = 500
#' )
#' }
#'
#' @export
posterior_draws_grid <- function(model,
                                xvar  = "vote_trump",
                                mvar  = "prepost",
                                xval  = c(0, 1),
                                mval  = c(0, 1),
                                covariates = NULL,
                                ndraws = NULL) {
  dat <- model$data
  hold_vars <- setdiff(names(dat)[-1], c(xvar, mvar))

  baseline <- dat %>%
    dplyr::select(dplyr::all_of(hold_vars)) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), mean))

  if (!is.null(covariates)) {
    for (nm in names(covariates)) {
      if (nm %in% names(baseline)) baseline[[nm]] <- covariates[[nm]]
    }
  }

  grid <- tidyr::expand_grid(baseline, !!xvar := xval, !!mvar := mval)
  tidybayes::add_epred_draws(grid, model, ndraws = ndraws)
}
