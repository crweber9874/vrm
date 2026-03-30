#' Summarize Random Effects from a BRMS Model
#'
#' Extracts and summarizes random-effect draws (group-level parameters) from a
#' \pkg{brms} model using \code{tidybayes::spread_draws}. Returns posterior
#' means and 95\% credible intervals per case.
#'
#' @param model1 A \code{brmsfit} object with random effects by \code{caseid}.
#' @param mean_name Character. Name for the posterior mean column.
#'   Default: \code{"mean_var"}.
#' @param lower_name Character. Name for the lower CI column.
#'   Default: \code{"lower.DIF"}.
#' @param upper_name Character. Name for the upper CI column.
#'   Default: \code{"upper.DIF"}.
#'
#' @return A tibble with one row per \code{caseid} and columns for the
#'   posterior mean, lower, and upper credible intervals.
#' @export
#'
#' @examples
#' \dontrun{
#' draws <- spreadDraw(fit, mean_name = "mean_re", lower_name = "lo", upper_name = "hi")
#' }
spreadDraw <- function(model1, mean_name = "mean_var", lower_name = "lower.DIF", upper_name = "upper.DIF") {
  result <- spread_draws(model1, r_caseid__eta[caseid, ]) %>%
    group_by(caseid) %>%
    summarize(
      !!mean_name := mean(r_caseid__eta),
      !!lower_name := quantile(r_caseid__eta, 0.025),
      !!upper_name := quantile(r_caseid__eta, 0.975)
    )
  return(result)
}
