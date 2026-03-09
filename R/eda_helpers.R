#' Summarize missingness in a data frame
#'
#' @param data A data.frame
#' @param vars Character vector of column names. If NULL, all columns are used.
#' @return A data.frame with columns Variable, N, Missing, Pct_Missing
#' @export
summarize_missingness <- function(data, vars = NULL) {
  if (is.null(vars)) vars <- names(data)
  sub <- data[, vars, drop = FALSE]
  data.frame(
    Variable    = vars,
    N           = vapply(sub, function(x) sum(!is.na(x)), integer(1)),
    Missing     = vapply(sub, function(x) sum(is.na(x)), integer(1)),
    Pct_Missing = round(100 * vapply(sub, function(x) mean(is.na(x)), numeric(1)), 2),
    stringsAsFactors = FALSE
  )
}

#' Descriptive statistics for a numeric vector
#'
#' @param x A numeric vector
#' @return A named numeric vector of summary statistics
#' @export
describe_numeric <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  skew <- if (n > 2 && s > 0) (n / ((n - 1) * (n - 2))) * sum(((x - m) / s)^3) else NA_real_
  c(N       = n,
    Mean    = m,
    SD      = s,
    Min     = min(x),
    Q1      = unname(quantile(x, 0.25)),
    Median  = median(x),
    Q3      = unname(quantile(x, 0.75)),
    Max     = max(x),
    Skewness = skew)
}

#' Pairwise correlation table
#'
#' Computes a correlation matrix and returns it as a long-format data.frame
#' suitable for plotting or display.
#'
#' @param data A data.frame (numeric columns only)
#' @param method One of "pearson", "spearman", "kendall"
#' @param use Passed to \code{cor()}, e.g. "pairwise.complete.obs"
#' @return A data.frame with columns Var1, Var2, r
#' @export
cor_table <- function(data, method = "pearson", use = "pairwise.complete.obs") {
  num_data <- data[, vapply(data, is.numeric, logical(1)), drop = FALSE]
  cm <- stats::cor(num_data, method = method, use = use)
  long <- as.data.frame(as.table(cm), stringsAsFactors = FALSE)
  names(long) <- c("Var1", "Var2", "r")
  long
}

#' Quick MICE imputation wrapper
#'
#' @param data A data.frame
#' @param vars Character vector of columns to impute. If NULL, all columns.
#' @param method Default imputation method (e.g. "pmm", "cart", "rf")
#' @param m Number of imputations
#' @param maxit Maximum iterations
#' @param seed Random seed
#' @return A \code{mids} object from \pkg{mice}
#' @export
impute_mice <- function(data, vars = NULL, method = "pmm",
                        m = 5, maxit = 5, seed = 42) {
  if (!requireNamespace("mice", quietly = TRUE))
    stop("Package 'mice' is required. Install with install.packages('mice').")
  if (!is.null(vars)) data <- data[, vars, drop = FALSE]
  mice::mice(data, m = m, maxit = maxit,
             defaultMethod = rep(method, 4),
             seed = seed, printFlag = FALSE)
}

#' Variable type summary
#'
#' Classifies each column of a data.frame by type, unique values, and
#' proportion missing.
#'
#' @param data A data.frame
#' @return A data.frame with columns Variable, Class, Unique, Pct_Missing
#' @export
variable_characteristics <- function(data) {
  data.frame(
    Variable    = names(data),
    Class       = vapply(data, function(x) class(x)[1], character(1)),
    Unique      = vapply(data, function(x) length(unique(x)), integer(1)),
    Pct_Missing = round(100 * vapply(data, function(x) mean(is.na(x)), numeric(1)), 2),
    stringsAsFactors = FALSE
  )
}
