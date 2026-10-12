#' Minimal ggplot2 Theme for Effect and Prediction Plots
#'
#' `theme_minimal()` with minor gridlines removed and the legend at the
#' bottom: the combination used across most effect plots in project repos.
#' A lighter-weight alternative to [mytheme()].
#'
#' @param base_size Base font size. Default `11`.
#' @param base_family Base font family. Default `""`.
#' @param legend_position Legend position passed to `ggplot2::theme()`.
#'   Default `"bottom"`.
#'
#' @return A `ggplot2` theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_vrm()
theme_vrm <- function(base_size = 11, base_family = "", legend_position = "bottom") {
  if (!is.numeric(base_size) || length(base_size) != 1 || base_size <= 0) {
    stop("`base_size` must be a single positive number.", call. = FALSE)
  }
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = legend_position,
      strip.text       = ggplot2::element_text(face = "bold"),
      plot.title       = ggplot2::element_text(face = "bold")
    )
}
