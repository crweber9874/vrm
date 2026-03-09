#' Plot Predicted Probabilities with Connected Sunflowers
#'
#' Creates a visualization showing predicted probabilities with points connected
#' to the y-axis via line segments, with sunflower plots centered at the mean
#' points showing the distribution of observations.
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
#' @param mean_point_size Size of mean point. Default: 3
#' @param mean_point_alpha Transparency of mean point. Default: 0.8
#' @param connector_linewidth Width of connector lines to y-axis. Default: 0.5
#' @param connector_alpha Transparency of connector lines. Default: 0.5
#' @param dodge_width Width for position dodging. Default: 0.5
#' @param sunflower_density Density parameter for sunflower positioning. Default: 50
#' @param sunflower_aspect_ratio Aspect ratio for sunflower positioning. Default: 5
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' plotConnectedSunflower(plot_dat, observed_counts)
#'
#' # With different x variable
#' plotConnectedSunflower(plot_dat, observed_counts,
#'                        x_var = "time_label",
#'                        join_vars = c("time", "treatment", "category"))
#'
#' # Custom styling
#' plotConnectedSunflower(plot_dat, observed_counts,
#'                        mean_point_size = 4,
#'                        connector_linewidth = 1,
#'                        sunflower_size = 0.1)
#' }
#'
#' @export
plotConnectedSunflower <- function(pred_summary,
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
                                   mean_point_size = 3,
                                   mean_point_alpha = 0.8,
                                   connector_linewidth = 0.5,
                                   connector_alpha = 0.5,
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
    tidyr::uncount(n) %>%
    dplyr::filter(!is.na(.data$mean_prob))

  # Manually calculate dodged x positions for vertical lines
  pred_summary_dodged <- pred_summary %>%
    dplyr::group_by(.data[[x_var]]) %>%
    dplyr::mutate(
      n_groups = dplyr::n(),
      group_id = dplyr::row_number(),
      x_numeric = as.numeric(as.factor(.data[[x_var]])),
      x_offset = (group_id - (n_groups + 1) / 2) * dodge_width / n_groups,
      x_dodged = x_numeric + x_offset
    ) %>%
    dplyr::ungroup()

  # Create x-axis label mapping
  x_labels_map <- pred_summary_dodged %>%
    dplyr::select(dplyr::all_of(x_var), x_numeric) %>%
    dplyr::distinct() %>%
    dplyr::arrange(x_numeric) %>%
    dplyr::mutate(x_label = as.character(.data[[x_var]]))

  # Create the base plot with dodged x positions
  p <- pred_summary_dodged %>%
    ggplot2::ggplot(ggplot2::aes(x = as.factor(x_dodged), y = mean_prob, color = category)) +
    # Vertical connector lines from y=0 to mean points
    ggplot2::geom_segment(ggplot2::aes(x = x_dodged, xend = x_dodged,
                                       y = 0, yend = mean_prob),
                          linewidth = connector_linewidth,
                          alpha = connector_alpha) +
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
    # Mean points at dodged positions
    ggplot2::geom_point(size = mean_point_size,
                        alpha = mean_point_alpha) +
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
    # ggplot2::scale_x_continuous(
    #   breaks = x_labels_map$x_numeric,
    #   labels = x_labels_map$x_label
    # ) +
    ggplot2::scale_color_manual(values = colors, labels = category_labels)

  return(p)
}
