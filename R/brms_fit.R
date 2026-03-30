#' Fit a brms regression model
#'
#' A convenience wrapper around \code{\link[brms]{brm}} that accepts
#' dependent variable, treatment, moderator, and controls as separate
#' character strings and builds the formula automatically. Supports any
#' brms family.
#'
#' @param data A data.frame. Required -- no default.
#' @param DV Character string naming the dependent variable.
#' @param treatment Character string naming the treatment variable(s).
#'   Multiple treatments can be separated with \code{+}
#'   (e.g., \code{"prepost + violent_treat"}).
#' @param moderator Optional character string naming the moderating variable.
#'   If supplied, an interaction \code{treatment * moderator} is created.
#'   Default \code{NULL}.
#' @param controls Optional character string of control variables separated
#'   by \code{+} (e.g., \code{"female + college + age"}). Default \code{NULL}.
#' @param IV Optional character string for the full right-hand side formula
#'   fragment. If supplied, \code{treatment}, \code{moderator}, and
#'   \code{controls} are ignored. This preserves backward compatibility.
#' @param family A \pkg{brms} family object. Defaults to \code{gaussian()}.
#'   Common choices: \code{cumulative("logit")}, \code{bernoulli()},
#'   \code{categorical()}, \code{gaussian()}.
#' @param chains Number of MCMC chains. Default 4.
#' @param iter Total iterations per chain. Default 2000.
#' @param warmup Warmup iterations. Default 1000.
#' @param cores Number of cores. Default 4.
#' @param weights Optional character string naming a column of observation
#'   weights in \code{data}. When supplied, the formula LHS becomes
#'   \code{DV | weights(wts_col)}. Default \code{NULL} (unweighted).
#' @param seed Random seed. Default 1234.
#' @param ... Additional arguments passed to \code{\link[brms]{brm}}.
#'
#' @return A \code{brmsfit} object.
#' @export
#'
#' @examples
#' \dontrun{
#' # Using treatment / moderator / controls (recommended)
#' m <- brms_fit(
#'   data      = wss20,
#'   DV        = "contestation_value",
#'   treatment = "prepost",
#'   moderator = "vote_trump",
#'   controls  = "female + latino + college + age"
#' )
#' # Produces: contestation_value ~ prepost * vote_trump + female + latino + college + age
#'
#' # With IPTW weights
#' m <- brms_fit(
#'   data      = wss20,
#'   DV        = "contestation_value",
#'   treatment = "prepost",
#'   moderator = "vote_trump",
#'   controls  = "female + latino + college + age",
#'   weights   = "wts"
#' )
#'
#' # Legacy IV syntax still works
#' m <- brms_fit(data = mtcars, DV = "mpg", IV = "hp + wt")
#' }
brms_fit <- function(data,
                     DV,
                     treatment = NULL,
                     moderator = NULL,
                     controls  = NULL,
                     IV        = NULL,
                     family    = gaussian(),
                     weights   = NULL,
                     chains = 4, iter = 2000, warmup = 1000,
                     cores = 4, seed = 1234, ...) {

  rhs <- build_formula_rhs(treatment, moderator, controls, IV)

  lhs <- DV
  if (!is.null(weights)) {
    if (!weights %in% names(data)) {
      stop("Weight column '", weights, "' not found in data.", call. = FALSE)
    }
    lhs <- paste0(DV, " | weights(", weights, ")")
  }

  model_formula <- stats::as.formula(paste(lhs, "~", rhs))

  cat("Formula:", deparse(model_formula), "\n")

  brms::brm(
    formula = model_formula,
    data    = data,
    family  = family,
    chains  = chains,
    iter    = iter,
    warmup  = warmup,
    cores   = cores,
    seed    = seed,
    ...
  )
}


#' Build formula right-hand side from components
#'
#' Assembles a formula RHS string from treatment, moderator, and controls.
#' If \code{IV} is supplied directly, it is returned as-is.
#'
#' @param treatment Character string of treatment variable(s).
#' @param moderator Character string of moderator variable, or NULL.
#' @param controls Character string of control variables, or NULL.
#' @param IV Character string for full RHS override, or NULL.
#'
#' @return A character string suitable for the RHS of a formula.
#' @keywords internal
#' @export
build_formula_rhs <- function(treatment = NULL,
                              moderator = NULL,
                              controls  = NULL,
                              IV        = NULL) {
  # Legacy path: IV overrides everything

  if (!is.null(IV)) return(IV)

  if (is.null(treatment)) {
    stop("Either 'treatment' or 'IV' must be specified.", call. = FALSE)
  }

  # Build: treatment * moderator + controls
  if (!is.null(moderator)) {
    core <- paste(treatment, "*", moderator)
  } else {
    core <- treatment
  }

  if (!is.null(controls)) {
    rhs <- paste(core, "+", controls)
  } else {
    rhs <- core
  }

  rhs
}
