#' Create Combined Dot Plot with Marginal Effects
#'
#' Creates a 2x2 panel plot with dot plots showing predictions and marginal effects
#' for two groups defined by a moderator variable.
#'
#' @param plot_data A list containing `plotting_data` and `observed_counts`
#' @param predictions Predictions object for calculating marginal effects (optional if me_data provided)
#' @param me_data Pre-calculated marginal effects data frame (optional, overrides predictions)
#' @param main_title Main title for the combined plot
#' @param subtitle Subtitle for the combined plot. Default: NULL
#' @param caption Caption/notes for the combined plot. Default: NULL
#' @param moderator Name of the moderator variable (as string). Default: "vote_trump"
#' @param moderator_levels Vector of two values indicating which levels to plot.
#'   Default: c(0, 1)
#' @param group_labels Character vector of length 2 with labels for each group.
#'   Default: c("Group 0", "Group 1")
#' @param category_var Name of the category variable. Default: "category"
#' @param category_levels Character vector of category levels. Default: c("1", "2", "3", "4", "5")
#' @param category_labels Character vector of category labels.
#'   Default: c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree")
#' @param focal_var Focal variable for marginal effects. Default: "prepost"
#' @param focal_contrast Contrast values for focal variable. Default: c(0, 1)
#' @param color_var Name of variable for coloring in dot plots. Default: "prepost"
#' @param color_levels Levels of color variable. Default: c(0, 1)
#' @param color_labels Labels for color variable. Default: c("Pre", "Post")
#' @param color_values Colors for each level. Default: c("grey", "black")
#' @param me_y_var Name of y variable in marginal effects. Default: "mean_effect"
#' @param me_x_var Name of x variable in marginal effects. Default: "category"
#' @param me_ci_lower Name of lower CI column in marginal effects. Default: NULL (auto-detect)
#' @param me_ci_upper Name of upper CI column in marginal effects. Default: NULL (auto-detect)
#' @param prob_y_var Name of y variable for probability plots. Default: "mean_prob"
#' @param prediction_params List of parameters for prediction dot plots.
#'   Default includes show_errorbar = FALSE and connector_lines = TRUE
#' @param marginal_params List of parameters for marginal effect plots.
#'   Default includes show_errorbar = TRUE to display confidence intervals
#' @param layout_widths Width ratios for patchwork layout. Default: c(0.9, 1)
#' @param layout_heights Height ratios for patchwork layout. Default: c(1, 1)
#' @param save_path Optional file path to save the plot
#' @param width Plot width for saving. Default: 8
#' @param height Plot height for saving. Default: 6
#'
#' @return A patchwork object with the combined plot
#'
#' @examples
#' \dontrun{
#' # Basic usage with Trump/Biden voters
#' create_combined_plot(
#'   plot_data = plot_burn,
#'   predictions = predictions_burn,
#'   main_title = "Support for Election Policies",
#'   moderator = "vote_trump",
#'   moderator_levels = c(0, 1),
#'   group_labels = c("Biden Voters", "Trump Voters")
#' )
#'
#' # With pre-calculated marginal effects
#' me <- calculate_marginal_effect(predictions_burn,
#'                                 focal_var = "prepost",
#'                                 focal_contrast = c(0, 1),
#'                                 moderator = "vote_trump")
#'
#' create_combined_plot(
#'   plot_data = plot_burn,
#'   me_data = me,
#'   main_title = "Support for Election Policies"
#' )
#'
#' # Custom prediction parameters
#' create_combined_plot(
#'   plot_data = plot_burn,
#'   predictions = predictions_burn,
#'   prediction_params = list(
#'     point_size = 4,
#'     connector_linewidth = 1.2,
#'     y_limits = c(0, 0.8)
#'   )
#' )
#' }
#'
#' @export
create_combined_dot_plot <- function(plot_data,
                                 predictions = NULL,
                                 me_data = NULL,
                                 main_title,
                                 subtitle = NULL,
                                 caption = NULL,
                                 moderator = "vote_trump",
                                 moderator_levels = c(0, 1),
                                 group_labels = c("Group 0", "Group 1"),
                                 category_var = "category",
                                 category_levels = c("1", "2", "3", "4", "5"),
                                 category_labels = c("Strongly Disagree", "Disagree",
                                                     "Neither Agree nor Disagree",
                                                     "Agree", "Strongly Agree"),
                                 focal_var = "prepost",
                                 focal_contrast = c(0, 1),
                                 color_var = "prepost",
                                 color_levels = c(0, 1),
                                 color_labels = c("Pre", "Post"),
                                 color_values = c("grey", "black"),
                                 me_y_var = "mean_effect",
                                 me_x_var = "category",
                                 me_ci_lower = NULL,
                                 me_ci_upper = NULL,
                                 prob_y_var = "mean_prob",
                                 prediction_params = list(
                                   point_size = 3,
                                   point_alpha = 0.8,
                                   connector_lines = TRUE,
                                   connector_linewidth = 1,
                                   connector_alpha = 0.6,
                                   show_errorbar = FALSE,
                                   y_limits = c(0, 1),
                                   dodge_width = 0.6
                                 ),
                                 marginal_params = list(
                                   point_size = 3,
                                   point_alpha = 0.8,
                                   connector_lines = FALSE,
                                   show_errorbar = TRUE,
                                   errorbar_width = 0.2,
                                   reference_line = 0,
                                   y_limits = c(-0.3, 0.3),
                                   dodge_width = 0.6
                                 ),
                                 layout_widths = c(0.9, 1),
                                 layout_heights = c(1, 1),
                                 save_path = NULL,
                                 width = 8,
                                 height = 6) {

  require(dplyr)
  require(patchwork)

  # Calculate marginal effects if not provided
  if (is.null(me_data)) {
    if (is.null(predictions)) {
      stop("Either predictions or me_data must be provided")
    }
    me_data <- calculate_marginal_effect(
      predictions,
      focal_var = focal_var,
      focal_contrast = focal_contrast,
      moderator = moderator
    )
  }

  # Filter plotting data for first group
  plot_data_group1 <- plot_data$plotting_data %>%
    dplyr::filter(.data[[moderator]] == moderator_levels[1])

  # Filter plotting data for second group
  plot_data_group2 <- plot_data$plotting_data %>%
    dplyr::filter(.data[[moderator]] == moderator_levels[2])

  # Create probability plot for first group
  group1_probs <- do.call(plotDots, c(
    list(
      plot_dat = plot_data_group1,
      y_var = prob_y_var,
      x_var = category_var,
      color_var = color_var,
      facet_var = NULL,
      category_levels = category_levels,
      category_labels = category_labels,
      color_levels = color_levels,
      color_labels = color_labels,
      color_values = color_values,
      title = group_labels[1],
      y_label = "Probability",
      x_label = ""
    ),
    prediction_params
  ))

  # Create probability plot for second group
  group2_probs <- do.call(plotDots, c(
    list(
      plot_dat = plot_data_group2,
      y_var = prob_y_var,
      x_var = category_var,
      color_var = color_var,
      facet_var = NULL,
      category_levels = category_levels,
      category_labels = category_labels,
      color_levels = color_levels,
      color_labels = color_labels,
      color_values = color_values,
      title = group_labels[2],
      y_label = "Probability",
      x_label = ""
    ),
    prediction_params
  ))

  # Create marginal effects plot for first group
  group1_margins <- do.call(plotDots, c(
    list(
      plot_dat = me_data %>% dplyr::filter(.data[[moderator]] == moderator_levels[1]),
      y_var = me_y_var,
      x_var = me_x_var,
      color_var = NULL,
      facet_var = NULL,
      category_levels = category_levels,
      category_labels = category_labels,
      ci_lower = me_ci_lower,
      ci_upper = me_ci_upper,
      title = "",
      y_label = "",
      x_label = ""
    ),
    marginal_params
  ))

  # Create marginal effects plot for second group
  group2_margins <- do.call(plotDots, c(
    list(
      plot_dat = me_data %>% dplyr::filter(.data[[moderator]] == moderator_levels[2]),
      y_var = me_y_var,
      x_var = me_x_var,
      color_var = NULL,
      facet_var = NULL,
      category_levels = category_levels,
      category_labels = category_labels,
      ci_lower = me_ci_lower,
      ci_upper = me_ci_upper,
      title = "",
      y_label = "",
      x_label = ""
    ),
    marginal_params
  ))

  # Remove legend from first probability plot
  group1_no_legend <- group1_probs +
    ggplot2::theme(legend.position = "none")

  # Combine with patchwork
  combined_plot <- group1_no_legend + group1_margins +
    group2_probs + group2_margins +
    patchwork::plot_layout(widths = layout_widths,
                           heights = layout_heights) +
    patchwork::plot_annotation(
      title = main_title,
      subtitle = subtitle,
      caption = caption,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11),
        plot.caption = ggplot2::element_text(size = 9, hjust = 0)
      )
    )

  # Save if path provided
  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, combined_plot, width = width, height = height)
  }

  return(combined_plot)
}
