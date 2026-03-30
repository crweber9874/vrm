#' Min-Max Normalize to Zero-One Range
#'
#' Rescales a numeric vector to the \code{[0, 1]} range using min-max
#' normalization.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector rescaled to \code{[0, 1]}.
#' @export
#'
#' @examples
#' zero.one(c(10, 20, 30, 40, 50))
#' # [1] 0.00 0.25 0.50 0.75 1.00
zero.one <- function(x) {
  min.x <- min(x, na.rm = TRUE)
  max.x <- max(x - min.x, na.rm = TRUE)
  return((x - min.x) / max.x)
}
