#' Common recoding themes
#'
#' @description
#' These are common recoding themes used in the electoralContestation package
#'
#' @export
five_r <- c("1" = 5, "2" = 4,
            "3" = 3, "4" = 2,
            "5" = 1)

#' @export
five_n <- c("1" = 1, "2" = 2,
            "3" = 3, "4" = 4,
            "5" = 5)

#' @export
four_r <- c("1" = 4, "2" = 3,
            "3" = 2, "4" = 1)

#' @export
four_n <- c("1" = 1, "2" = 2,
            "3" = 3, "4" = 4)
#' @export
five_mr <- c("1" = 5, "2" = 4,
            "5" = 3, "3" = 2,
            "4" = 1)

#' @export
five_mn <- c("1" = 1, "2" = 2,
             "5" = 3, "3" = 4,
             "4" = 5)

#' Rescale variable to 0-1 range
#'
#' @description
#' Rescales a numeric vector to range from 0 to 1 using min-max normalization
#'
#' @param x A numeric vector
#' @param na.rm Logical; should missing values be removed? Default is TRUE
#' @return A numeric vector rescaled to 0-1 range
#' @export
zero_one <- function(x, na.rm = TRUE) {
  (x - min(x, na.rm = na.rm)) / (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
}
