#' Posterior Predictive Marginal Effect
#'
#' Computes the average marginal effect of a focal variable from a \pkg{brms}
#' model, optionally conditioned on levels of a moderator. Supports ordinal,
#' multinomial, binary, and Gaussian families.
#'
#' @param model A \code{brmsfit} object.
#' @param xvar Character. Name of the focal (treatment) variable.
#' @param mvar Character. Name of the moderator variable.
#' @param mrange Numeric vector. Values of \code{mvar} to condition on.
#'   Default: \code{c(0, 1)}.
#' @param xrange Numeric vector of length 2. Low and high values of
#'   \code{xvar} to contrast. Default: \code{c(0, 1)}.
#'
#' @return A tibble with the mean marginal effect and 95\% credible intervals,
#'   grouped by moderator (and \code{.category} for ordinal/multinomial models).
#' @export
#'
#' @examples
#' \dontrun{
#' me <- posterior_pme(fit, xvar = "prepost", mvar = "vote_trump")
#' }
posterior_pme <- function(model = burn_flag,
                          xvar = "prepost",
                          mvar = "vote_trump",
                          mrange = c(0, 1),
                          xrange = c(0, 1)) {
  data <- model$data
  cols_to_average <- setdiff(names(data)[-1], c(xvar, mvar))

  data_grid <- data %>%
    dplyr::select(dplyr::all_of(cols_to_average)) %>%
    dplyr::summarize(dplyr::across(dplyr::everything(), mean)) %>%
    tidyr::expand_grid(
      !!xvar := xrange,
      !!mvar := mrange
    ) %>%
    tidybayes::add_epred_draws(model)

  x_hi <- xrange[2]
  x_lo <- xrange[1]

  t1 <-
    data_grid %>%
    dplyr::filter(!!rlang::sym(xvar) == x_hi) %>%
    subset(select = ".epred")

  t2 <- data_grid %>%
    dplyr::filter(!!rlang::sym(xvar) == x_lo) %>%
    subset(select = ".epred")

  dat <- data_grid %>%
    dplyr::filter(!!rlang::sym(xvar) == x_hi)

  dat$me <- t1$.epred - t2$.epred

  if (model$family[[1]] == "categorical" |
    model$family[[1]] == "cumulative") {
    plot <- dat %>%
      subset(select = c(mvar, ".category", "me")) %>%
      dplyr::group_by(!!rlang::sym(mvar), .category)
  }
  if (model$family[[1]] == "bernoulli" |
    model$family[[1]] == "gaussian") {
    plot <- dat %>%
      subset(select = c(mvar, "me")) %>%
      dplyr::group_by(!!rlang::sym(mvar))
  }

  plot <- plot %>%
    dplyr::summarize(
      mean = mean(me),
      lower = stats::quantile(me, 0.025),
      upper = stats::quantile(me, 0.975)
    )
  return(plot)
}
