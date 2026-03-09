#' Plot Predicted Probabilities with Sunflower Distribution
#'
#' Creates a visualization showing predicted probabilities across categories,
#' with sunflower plots indicating the distribution of observations and error bars
#' showing confidence intervals. The function handles joining prediction summaries
#' with observed counts internally.
#'
#' @param pred_summary A data frame with prediction summaries containing columns:
#'   \code{category}, \code{mean_prob}, \code{lower_ci}, \code{upper_ci}, and a
#'   label column for x-axis (specified by \code{x_var})
#' @param observed_counts A data frame with observed counts containing columns:
#'   \code{category}, \code{n}, and matching variables for joining
#' @param x_var Name of the x-axis variable with labels (as string). Default: "prepost_label"
#' @param join_vars Character vector of variable names to use for joining pred_summary
#'   and observed_counts. Default: c("prepost", "category")
#' @param colors Named vector of colors for each category level.
#'   Default: c("1" = "lightgrey", "2" = "grey", "3" = "darkgrey",
#'             "4" = "darkslategrey", "5" = "black")
#' @param category_labels Named vector of labels for each category level.
#'   Default: c("1" = "Strongly Not Support", "2" = "Do Not Support",
#'             "3" = "Neutral", "4" = "Support", "5" = "Strongly Support")
#' @param title Main title for the plot. Default: ""
#' @param subtitle Subtitle for the plot. Default: ""
#' @param y_label Label for the y-axis. Default: "Predicted Probability"
#' @param x_label Label for the x-axis. Default: ""
#' @param legend_title Title for the legend. Default: "Response\nCategory"
#' @param y_limits Numeric vector of length 2 specifying y-axis limits.
#'   Default: c(0, 0.5)
#' @param y_breaks Numeric vector specifying y-axis break points.
#'   Default: seq(0, 0.5, 0.1)
#' @param sunflower_size Size of sunflower plot points. Default: 0.2
#' @param sunflower_alpha Transparency of sunflower plot points. Default: 0.25
#' @param errorbar_width Width of error bars. Default: 0.1
#' @param errorbar_linewidth Line width of error bars. Default: 0.8
#' @param line_linewidth Line width for connecting lines. Default: 1
#' @param dodge_width Width for position dodging (applies to sunflowers, lines, and error bars). Default: 0.5
#' @param sunflower_density Density parameter for sunflower positioning. Default: 50
#' @param sunflower_aspect_ratio Aspect ratio for sunflower positioning. Default: 5
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' # Basic usage with prepost data
#' plotSunflower(plot_dat, observed_counts)
#'
#' # With different x variable
#' plotSunflower(plot_dat, observed_counts,
#'               x_var = "time_label",
#'               join_vars = c("time", "treatment", "category"))
#'
#' # Custom colors
#' plotSunflower(plot_dat, observed_counts,
#'               colors = c("1" = "#FF6B6B", "2" = "#4ECDC4",
#'                         "3" = "#45B7D1", "4" = "#FFA07A", "5" = "#98D8C8"))
#' }
#'
#' @export
plotSunflower <- function(pred_summary,
                          observed_counts,
                          x_var = "prepost_label",
                          join_vars = c("prepost", "category"),
                          colors = c("1" = "lightgrey", "2" = "grey", "3" = "darkgrey",
                                     "4" = "darkslategrey", "5" = "black"),
                          category_labels = c("1" = "Strongly Not Support",
                                              "2" = "Do Not Support",
                                              "3" = "Neutral",
                                              "4" = "Support",
                                              "5" = "Strongly Support"),
                          title = "",
                          subtitle = "",
                          y_label = "Predicted Probability",
                          x_label = "",
                          legend_title = "Response\nCategory",
                          y_limits = c(0, 0.5),
                          y_breaks = seq(0, 0.5, 0.1),
                          sunflower_size = 0.2,
                          sunflower_alpha = 0.25,
                          errorbar_width = 0.1,
                          errorbar_linewidth = 0.8,
                          line_linewidth = 1,
                          dodge_width = 0.5,
                          sunflower_density = 50,
                          sunflower_aspect_ratio = 5) {

  # Prepare sunflower data by joining observed_counts with predictions
  # and expanding counts into individual observations
  sunflower_data <- observed_counts %>%
    dplyr::left_join(
      pred_summary %>% dplyr::select(dplyr::all_of(c(join_vars, x_var, "mean_prob"))),
      by = join_vars
    ) %>%
    tidyr::uncount(n)  |>
    dplyr::filter(!is.na(.data$mean_prob))

  # Create the base plot
  p <- pred_summary %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[x_var]], y = mean_prob, color = category, group = category)) +
    # Sunflowers centered at predicted probabilities
    ggplot2::geom_point(data = sunflower_data,
                        ggplot2::aes(x = .data[[x_var]], y = mean_prob, color = category),
                        position = suppressMessages(
                        vayr::position_sunflowerdodge(width = dodge_width,
                                                                 density = sunflower_density,
                                                                 aspect_ratio = sunflower_aspect_ratio)),
                        size = sunflower_size,
                        alpha = sunflower_alpha,
                        inherit.aes = FALSE) +
    # Add error bars, lines, and ribbons
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower_ci, ymax = upper_ci),
                           position = ggplot2::position_dodge(width = dodge_width),
                           width = errorbar_width,
                           linewidth = errorbar_linewidth,
                           alpha = 0.7) +
    ggplot2::geom_line(position = ggplot2::position_dodge(width = dodge_width),
                       linewidth = line_linewidth) +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      color = legend_title,
      title = title,
      subtitle = subtitle
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 11, face = "bold"),
      text = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 7),
      axis.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 6),
      legend.title = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 12),
      plot.subtitle = ggplot2::element_text(size = 10)
    ) +
    ggplot2::scale_y_continuous(limits = y_limits, breaks = y_breaks) +
    ggplot2::scale_color_manual(values = colors, labels = category_labels) +
    ggplot2::scale_fill_manual(values = colors, labels = category_labels, guide = "none")

  return(p)
}
