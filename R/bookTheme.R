#' Common ggplot2 theme for publication-quality figures
#'
#' A minimal ggplot2 theme with bold axis text, no ticks, and a white
#' background. Suitable for academic manuscripts.
#'
#' @param ... Additional arguments (currently unused).
#' @return A \code{ggplot2} theme object.
#' @export
mytheme <- function(...){
  ggtheme =
    theme(
      plot.title =  ggplot2::element_text(face = "bold", hjust = 0, vjust = 0, colour = "#3C3C3C", size = 20),
      axis.text.x = ggplot2::element_text(size = 16, colour = "#535353", face = "bold"),
      axis.text.y = ggplot2::element_text(size = 16, colour = "#535353", face = "bold"),
      axis.title =  ggplot2::element_text(size = 16, colour = "#535353", face = "bold"),
      axis.title.y = ggplot2::element_text(size = 16, colour = "#535353", face = "bold", vjust = 1.5),
      axis.ticks = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(size = 16),
      panel.grid.major = ggplot2::element_line(colour = "#D0D0D0", linewidth = .25),
      panel.background = ggplot2::element_rect(fill = "white"),
      legend.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 16) )
  return(ggtheme)
}
