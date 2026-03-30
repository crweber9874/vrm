#' Reverse-coded 5-point Likert scale
#'
#' Named numeric vector that reverses a 1-5 scale (1→5, 2→4, ..., 5→1).
#'
#' @format A named numeric vector of length 5.
#' @export
five_r <- c(
  "1" = 5, "2" = 4,
  "3" = 3, "4" = 2,
  "5" = 1
)

#' Normal-coded 5-point Likert scale
#'
#' Named numeric vector preserving a 1-5 scale (identity mapping).
#'
#' @format A named numeric vector of length 5.
#' @export
five_n <- c(
  "1" = 1, "2" = 2,
  "3" = 3, "4" = 4,
  "5" = 5
)

#' Reverse-coded 4-point Likert scale
#'
#' Named numeric vector that reverses a 1-4 scale (1→4, 2→3, 3→2, 4→1).
#'
#' @format A named numeric vector of length 4.
#' @export
four_r <- c(
  "1" = 4, "2" = 3,
  "3" = 2, "4" = 1
)

#' Normal-coded 4-point Likert scale
#'
#' Named numeric vector preserving a 1-4 scale (identity mapping).
#'
#' @format A named numeric vector of length 4.
#' @export
four_n <- c(
  "1" = 1, "2" = 2,
  "3" = 3, "4" = 4
)

#' Reverse-coded 5-point scale with middle swap
#'
#' Named numeric vector that reverses a 5-point scale and moves the
#' middle category (original 5 → 3, original 3 → 2).
#'
#' @format A named numeric vector of length 5.
#' @export
five_mr <- c(
  "1" = 5, "2" = 4,
  "5" = 3, "3" = 2,
  "4" = 1
)

#' Normal-coded 5-point scale with middle swap
#'
#' Named numeric vector that reorders a 5-point scale moving the
#' middle category (original 5 → 3, original 3 → 4).
#'
#' @format A named numeric vector of length 5.
#' @export
five_mn <- c(
  "1" = 1, "2" = 2,
  "5" = 3, "3" = 4,
  "4" = 5
)

#' Normal-coded 7-point scale
#'
#' Named numeric vector preserving a 1-7 scale (identity mapping).
#'
#' @format A named numeric vector of length 7.
#' @export
seven_n <- c(
  "1" = 1, "2" = 2, "3" = 3,
  "4" = 4, "5" = 5, "6" = 6,
  "7" = 7
)

#' Normal-coded 3-point scale
#'
#' Named numeric vector preserving a 1-3 scale (identity mapping).
#'
#' @format A named numeric vector of length 3.
#' @export
three_n <- c(
  "1" = 1, "2" = 2, "3" = 3
)

#' Binary recode (1=1, 2=0)
#'
#' Named numeric vector for recoding 1/2 survey responses to 1/0.
#'
#' @format A named numeric vector of length 2.
#' @export
binary_10 <- c("1" = 1, "2" = 0)

#' Binary recode reversed (1=0, 2=1)
#'
#' Named numeric vector for recoding 1/2 survey responses to 0/1.
#'
#' @format A named numeric vector of length 2.
#' @export
binary_01 <- c("1" = 0, "2" = 1)

#' Rescale Variable to 0-1 Range
#'
#' Rescales a numeric vector to range from 0 to 1 using min-max normalization.
#'
#' @param x A numeric vector.
#' @param na.rm Logical; should missing values be removed? Default is TRUE.
#'
#' @return A numeric vector rescaled to \code{[0, 1]}.
#' @export
#'
#' @examples
#' zero_one(c(10, 20, 30, 40, 50))
#' # [1] 0.00 0.25 0.50 0.75 1.00
zero_one <- function(x, na.rm = TRUE) {
  (x - min(x, na.rm = na.rm)) / (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
}
