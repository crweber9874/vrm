#' Row Means for Multi-Item Scales
#'
#' Averages items row-wise for scale construction, returning `NA` (not `NaN`)
#' when too few items are observed. Replaces the common
#' `rowMeans(cbind(...), na.rm = TRUE)` idiom, which silently returns `NaN`
#' for all-missing rows and averages over a single item when others are missing.
#'
#' @param ... Numeric vectors of equal length, or a single data frame / matrix
#'   (e.g., `dplyr::pick(auth_1:auth_4)` inside `mutate()`).
#' @param min_valid Minimum number of non-missing items required for a row
#'   to receive a score. Values in `(0, 1)` are interpreted as a proportion of
#'   items. Default `1`.
#'
#' @return A numeric vector of row means.
#' @export
#'
#' @examples
#' row_mean(c(1, 2, NA), c(3, NA, NA))
#' row_mean(c(1, 2, NA), c(3, NA, NA), min_valid = 2)
#'
#' df <- data.frame(a = c(1, NA, 3), b = c(2, NA, 5), c = c(3, 4, NA))
#' dplyr::mutate(df, scale = row_mean(dplyr::pick(a:c), min_valid = 0.5))
row_mean <- function(..., min_valid = 1) {
  args <- list(...)
  if (length(args) == 0) stop("`row_mean()` requires at least one input.", call. = FALSE)

  m <- if (length(args) == 1 && (is.data.frame(args[[1]]) || is.matrix(args[[1]]))) {
    as.matrix(args[[1]])
  } else {
    lens <- lengths(args)
    if (length(unique(lens)) != 1) {
      stop("All inputs to `row_mean()` must have the same length; got ",
           paste(unique(lens), collapse = ", "), ".", call. = FALSE)
    }
    do.call(cbind, args)
  }

  if (!is.numeric(m) && !is.logical(m)) {
    stop("`row_mean()` requires numeric inputs.", call. = FALSE)
  }
  if (!is.numeric(min_valid) || length(min_valid) != 1 || is.na(min_valid) || min_valid <= 0) {
    stop("`min_valid` must be a single positive number.", call. = FALSE)
  }

  k <- ncol(m)
  threshold <- if (min_valid < 1) ceiling(min_valid * k) else min_valid
  if (threshold > k) {
    stop("`min_valid` (", threshold, ") exceeds the number of items (", k, ").", call. = FALSE)
  }

  n_obs <- rowSums(!is.na(m))
  out <- rowMeans(m, na.rm = TRUE)
  out[n_obs < threshold] <- NA_real_
  unname(out)
}
