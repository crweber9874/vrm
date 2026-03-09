#' Propensity Score Weighting
#'
#' @description
#' Estimates propensity scores and computes stabilized inverse probability weights (IPW).
#' Supports logistic regression, gradient boosted models, and random forest.
#' When a single method is used, columns are named \code{ps} and \code{wts}.
#' When multiple methods are used, columns are suffixed: \code{ps_glm}, \code{wts_glm}, etc.
#'
#' @param data A dataframe containing the treatment and covariates
#' @param treatment Character string naming the binary treatment column (0/1)
#' @param formula A one-sided formula for covariates (e.g., \code{~ female + college + scale(age)})
#' @param methods Character vector of methods to run. Any combination of \code{"glm"},
#'   \code{"gbm"}, and \code{"rf"}. Default is all three.
#' @param gbm_trees Number of trees for GBM. Default is 1000.
#' @param gbm_depth Interaction depth for GBM. Default is 3.
#' @param rf_ntree Number of trees for random forest. Default is 1000.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{data}{The input dataframe with new \code{ps} and \code{wts} columns
#'       (suffixed by method name when multiple methods are used).}
#'     \item{fits}{A named list of fitted model objects, keyed by method name.}
#'   }
#'
#' @export
ps_weight <- function(data, treatment, formula,
                      methods = c("glm", "gbm", "rf"),
                      gbm_trees = 1000, gbm_depth = 3,
                      rf_ntree = 1000) {
  methods <- match.arg(methods, c("glm", "gbm", "rf"), several.ok = TRUE)
  treat <- data[[treatment]]
  den <- mean(treat, na.rm = TRUE)
  use_suffix <- length(methods) > 1
  fits <- list()

  full_formula <- stats::reformulate(
    attr(stats::terms(formula), "term.labels"),
    response = treatment
  )

  make_wts <- function(ps) {
    ifelse(treat == 1, den / ps, (1 - den) / (1 - ps))
  }

  if ("glm" %in% methods) {
    fit <- stats::glm(full_formula, data = data,
                      family = stats::binomial("logit"))
    fits$glm <- fit
    ps <- stats::predict(fit, newdata = data, type = "response")
    if (use_suffix) {
      data$ps_glm <- ps
      data$wts_glm <- make_wts(ps)
    } else {
      data$ps <- ps
      data$wts <- make_wts(ps)
    }
  }

  if ("gbm" %in% methods) {
    if (!requireNamespace("gbm", quietly = TRUE)) {
      stop("Package 'gbm' is required. Install with install.packages('gbm').")
    }
    fit <- gbm::gbm(full_formula, data = data,
                     distribution = "bernoulli",
                     n.trees = gbm_trees,
                     interaction.depth = gbm_depth,
                     verbose = FALSE)
    fits$gbm <- fit
    ps <- gbm::predict.gbm(fit, newdata = data,
                            n.trees = gbm_trees, type = "response")
    if (use_suffix) {
      data$ps_gbm <- ps
      data$wts_gbm <- make_wts(ps)
    } else {
      data$ps <- ps
      data$wts <- make_wts(ps)
    }
  }

  if ("rf" %in% methods) {
    if (!requireNamespace("randomForest", quietly = TRUE)) {
      stop("Package 'randomForest' is required. Install with install.packages('randomForest').")
    }
    rf_data <- data
    rf_data[[treatment]] <- as.factor(rf_data[[treatment]])
    fit <- randomForest::randomForest(full_formula,
                                      data = rf_data,
                                      ntree = rf_ntree)
    fits$rf <- fit
    ps <- stats::predict(fit, type = "prob")[, "1"]
    if (use_suffix) {
      data$ps_rf <- ps
      data$wts_rf <- make_wts(ps)
    } else {
      data$ps <- ps
      data$wts <- make_wts(ps)
    }
  }

  list(data = data, fits = fits)
}
