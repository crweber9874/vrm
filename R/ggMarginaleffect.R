#' Plot Marginal Effects with Error Bars
#'
#' Creates a visualization showing marginal effects for ordered categories with
#' confidence intervals displayed as horizontal error bars.
#'
#' @param me_data A data frame with marginal effects containing columns:
#'   \code{category}, \code{mean_effect}, \code{lower_ci}, \code{upper_ci}
#' @param category_labels Named vector of labels for each category level.
#'   Default: c("1" = "Strongly Not Support", "2" = "Do Not Support",
#'             "3" = "Neutral", "4" = "Support", "5" = "Strongly Support")
#' @param title Main title for the plot.
#'   Default: "Marginal Effects"
#' @param x_label Label for the x-axis. Default: "Estimated Effect and Confidence Interval"
#' @param y_label Label for the y-axis. Default: "Category"
#' @param x_limits Numeric vector of length 2 specifying x-axis limits.
#'   Default: c(-0.2, 0.2)
#' @param x_breaks Numeric vector specifying x-axis break points.
#'   Default: seq(-0.2, 0.2, by = 0.1)
#' @param point_size Size of the effect estimate points. Default: 5
#' @param point_color Color of the effect estimate points. Default: "black"
#' @param point_alpha Transparency of the effect estimate points. Default: 0.5
#' @param errorbar_color Color of the error bars. Default: "darkgrey"
#' @param errorbar_height Height of the error bars. Default: 0.05
#' @param vline_color Color of the reference line at zero. Default: "darkgrey"
#' @param vline_linetype Line type for the reference line. Default: "dashed"
#' @param vline_size Line width for the reference line. Default: 0.5
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' plotMarginalEffects(me_data)
#'
#' # Filtered to Biden voters only
#' plotMarginalEffects(me %>% filter(vote_trump == 0),
#'                     title = "Marginal Effects (Biden Voters)")
#'
#' # Custom colors and styling
#' plotMarginalEffects(me_data,
#'                     point_color = "blue",
#'                     errorbar_color = "navy",
#'                     x_limits = c(-0.3, 0.3))
#' }
#'
#' @export
plotMarginalEffects <- function(me_data,
                                category_labels = c("1" = "Strongly Oppose",
                                                    "2" = "Oppose",
                                                    "3" = "Neutral",
                                                    "4" = "Support",
                                                    "5" = "Strongly Support"),
                                title = "Marginal Effects",
                                x_label = "Estimated Effect and Confidence Interval",
                                y_label = "Category",
                                x_limits = c(-0.2, 0.2),
                                x_breaks = seq(-0.2, 0.2, by = 0.1),
                                point_size = 5,
                                point_color = "black",
                                point_alpha = 0.5,
                                errorbar_color = "darkgrey",
                                errorbar_height = 0.05,
                                vline_color = "darkgrey",
                                vline_linetype = "dashed",
                                vline_size = 0.5) {

  p <- me_data %>%
    ggplot2::ggplot(ggplot2::aes(y = category, x = mean_effect)) +
    ggplot2::geom_vline(xintercept = 0,
                        linetype = vline_linetype,
                        color = vline_color,
                        size = vline_size) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower_ci, xmax = upper_ci),
                            height = errorbar_height,
                            color = errorbar_color) +
    ggplot2::geom_point(size = point_size,
                        color = point_color,
                        alpha = point_alpha) +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      title = title
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 12, face = "bold"),
      text = element_text(size = 8),           #
      axis.text = element_text(size = 7),
      axis.title = element_text(size = 9),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      plot.title = element_text(size = 11),
      plot.subtitle = element_text(size = 11),
      axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1)
    ) +
    ggplot2::scale_x_continuous(limits = x_limits, breaks = x_breaks) +
    ggplot2::scale_y_discrete(labels = category_labels)

  return(p)
}
