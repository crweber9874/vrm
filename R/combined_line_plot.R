#' Create Combined Sunflower and Marginal Effects Plot
#'
#' Creates a 2x2 panel plot with sunflower plots (using plotSunflower) showing
#' predicted probabilities with error bars and connecting lines, plus marginal
#' effects plots for two groups defined by a moderator variable. Left panels show
#' predicted probabilities with sunflower distributions, right panels show marginal
#' effects with confidence intervals.
#'
#' @param plot_data A list containing two elements:
#'   \describe{
#'     \item{plotting_data}{Data frame with prediction summaries (mean_prob, lower_ci, upper_ci)}
#'     \item{observed_counts}{Data frame with observed counts (n) for each category combination}
#'   }
#' @param predictions Predictions object (long format) for calculating marginal effects.
#'   Optional if me_data is provided. Must contain 'draw' column.
#' @param me_data Pre-calculated marginal effects data frame. Optional, overrides predictions.
#'   Should contain mean_effect, lower_ci, upper_ci columns.
#' @param main_title Character. Main title for the combined plot.
#' @param subtitle Character. Subtitle for the combined plot. Default: NULL
#' @param caption Character. Caption/notes for the combined plot. Default: NULL
#' @param moderator Character. Name of the moderator variable (as string).
#'   This variable defines the two groups to compare. Default: "vote_trump"
#' @param moderator_levels Vector of length 2. Values of moderator variable indicating
#'   which levels to plot. Default: c(0, 1)
#' @param group_labels Character vector of length 2. Labels for each group/panel.
#'   Default: c("Group 0", "Group 1")
#' @param category_var Character. Name of the category variable. Default: "category"
#' @param category_levels Character vector. Underlying levels of category variable
#'   (as they appear in data). Default: c("1", "2", "3", "4", "5")
#' @param category_labels Character vector. Display labels for categories.
#'   Default: c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree",
#'              "Agree", "Strongly Agree")
#' @param x_var Character. Name of the x-axis variable with labels (e.g., "prepost" or
#'   "prepost_label"). Default: "prepost"
#' @param join_vars Character vector. Variable names to use for joining pred_summary
#'   and observed_counts. Must include all grouping variables.
#'   Default: c("prepost", "category")
#' @param focal_var Character. Focal variable for marginal effects calculation.
#'   Default: "prepost"
#' @param focal_contrast Vector of length 2. Values of focal_var to contrast when
#'   calculating marginal effects. Effect = focal_contrast[2] - focal_contrast[1].
#'   Default: c(0, 1)
#' @param colors Named vector. Colors for each category level. Names should match
#'   category_levels. Default: c("1" = "lightgrey", "2" = "grey", "3" = "darkgrey",
#'                                "4" = "darkslategrey", "5" = "black")
#' @param me_y_var Character. Name of y variable in marginal effects data.
#'   Default: "mean_effect"
#' @param me_x_var Character. Name of x variable in marginal effects data.
#'   Default: "category"
#' @param me_ci_lower Character. Name of lower CI column in marginal effects.
#'   Default: NULL (auto-detects "lower_ci" or "lower")
#' @param me_ci_upper Character. Name of upper CI column in marginal effects.
#'   Default: NULL (auto-detects "upper_ci" or "upper")
#' @param sunflower_params List. Parameters for sunflower plots passed to plotSunflower.
#'   Default list includes:
#'   \describe{
#'     \item{sunflower_size}{Size of sunflower points (default: 0.2)}
#'     \item{sunflower_alpha}{Transparency of sunflower points (default: 0.25)}
#'     \item{errorbar_width}{Width of error bars (default: 0.1)}
#'     \item{errorbar_linewidth}{Line width of error bars (default: 0.8)}
#'     \item{line_linewidth}{Line width for connecting lines (default: 1)}
#'     \item{dodge_width}{Dodging width for position adjustment (default: 0.5)}
#'     \item{sunflower_density}{Density parameter for sunflower positioning (default: 50)}
#'     \item{sunflower_aspect_ratio}{Aspect ratio for sunflower positioning (default: 5)}
#'     \item{y_limits}{Y-axis limits as numeric vector (default: c(0, 0.5))}
#'     \item{y_breaks}{Y-axis break points (default: seq(0, 0.5, 0.1))}
#'   }
#' @param marginal_params List. Parameters for marginal effect plots passed to plotDots.
#'   Default list includes:
#'   \describe{
#'     \item{point_size}{Size of effect estimate points (default: 3)}
#'     \item{point_alpha}{Transparency of points (default: 0.8)}
#'     \item{connector_lines}{Whether to draw connector lines (default: FALSE)}
#'     \item{show_errorbar}{Whether to show error bars (default: TRUE)}
#'     \item{errorbar_width}{Width of error bars (default: 0.2)}
#'     \item{reference_line}{Y-value for reference line (default: 0)}
#'     \item{y_limits}{Y-axis limits (default: c(-0.3, 0.3))}
#'     \item{dodge_width}{Dodging width (default: 0.6)}
#'   }
#' @param layout_widths Numeric vector of length 2. Width ratios for patchwork layout.
#'   Default: c(0.9, 1)
#' @param layout_heights Numeric vector of length 2. Height ratios for patchwork layout.
#'   Default: c(1, 1)
#' @param save_path Character. Optional file path to save the plot. Default: NULL
#' @param width Numeric. Plot width in inches for saving. Default: 8
#' @param height Numeric. Plot height in inches for saving. Default: 6
#'
#' @return A patchwork object containing the combined 2x2 plot. Can be further
#'   customized using patchwork operators or displayed directly.
#'
#' @details
#' This function creates a 2x2 panel layout comparing two groups:
#' \itemize{
#'   \item Top-left: Sunflower plot for group 1 (no legend)
#'   \item Top-right: Marginal effects for group 1
#'   \item Bottom-left: Sunflower plot for group 2 (with legend)
#'   \item Bottom-right: Marginal effects for group 2
#' }
#'
#' The sunflower plots show predicted probabilities with connecting lines between
#' conditions (e.g., pre to post), error bars for uncertainty, and sunflower points
#' showing the distribution of observed responses. The marginal effects panels show
#' the difference in predicted probabilities between conditions with confidence intervals.
#'
#' @examples
#' \dontrun{
#' # Fit ordered logistic regression model
#' library(MASS)
#' model <- polr(outcome ~ prepost * vote_trump + age + education,
#'               data = survey_data, method = "logistic", Hess = TRUE)
#'
#' # Generate predictions
#' design <- expand.grid(
#'   prepost = c(0, 1),
#'   vote_trump = c(0, 1),
#'   age = mean(survey_data$age),
#'   education = mean(survey_data$education)
#' )
#'
#' predictions <- predict_ordinal_probs(
#'   design_matrix = design,
#'   model = model,
#'   n_draws = 1000,
#'   category_labels = c("1", "2", "3", "4", "5")
#' )
#'
#' # Prepare plot data
#' plot_data <- prepare_prediction_data(
#'   data = survey_data,
#'   predictions = predictions,
#'   category_var = "outcome",
#'   group_vars = c("prepost", "vote_trump")
#' )
#'
#' # Basic usage
#' create_combined_sunflower(
#'   plot_data = plot_data,
#'   predictions = predictions,
#'   main_title = "Policy Support Analysis",
#'   subtitle = "Pre vs Post Treatment by Voter Group",
#'   moderator = "vote_trump",
#'   moderator_levels = c(0, 1),
#'   group_labels = c("Biden Voters", "Trump Voters")
#' )
#'
#' # With custom styling
#' create_combined_sunflower(
#'   plot_data = plot_data,
#'   predictions = predictions,
#'   main_title = "Policy Support (Custom)",
#'   group_labels = c("Biden Voters", "Trump Voters"),
#'   sunflower_params = list(
#'     sunflower_size = 0.3,
#'     sunflower_alpha = 0.4,
#'     line_linewidth = 1.5,
#'     y_limits = c(0, 0.6)
#'   ),
#'   marginal_params = list(
#'     point_size = 4,
#'     errorbar_width = 0.3,
#'     y_limits = c(-0.4, 0.4)
#'   )
#' )
#'
#' # Save to file
#' create_combined_sunflower(
#'   plot_data = plot_data,
#'   predictions = predictions,
#'   main_title = "Policy Support Analysis",
#'   group_labels = c("Biden Voters", "Trump Voters"),
#'   save_path = "policy_support.png",
#'   width = 10,
#'   height = 7
#' )
#' }
#'
#' @seealso
#' \code{\link{plotSunflower}} for the individual sunflower plots,
#' \code{\link{plotDots}} for the marginal effects plots,
#' \code{\link{calculate_marginal_effect}} for computing marginal effects,
#' \code{\link{prepare_prediction_data}} for preparing input data
#'
#' @export
create_combined_sunflower_plot <- function(plot_data,
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
                                      x_var = "prepost",
                                      join_vars = c("prepost", "category"),
                                      focal_var = "prepost",
                                      focal_contrast = c(0, 1),
                                      colors = c("1" = "lightgrey", "2" = "grey",
                                                 "3" = "darkgrey", "4" = "darkslategrey",
                                                 "5" = "black"),
                                      me_y_var = "mean_effect",
                                      me_x_var = "category",
                                      me_ci_lower = NULL,
                                      me_ci_upper = NULL,
                                      sunflower_params = list(
                                        sunflower_size = 0.2,
                                        sunflower_alpha = 0.25,
                                        errorbar_width = 0.1,
                                        errorbar_linewidth = 0.8,
                                        line_linewidth = 1,
                                        dodge_width = 0.5,
                                        sunflower_density = 50,
                                        sunflower_aspect_ratio = 5,
                                        y_limits = c(0, 0.5),
                                        y_breaks = seq(0, 0.5, 0.1)
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

  pred_summary_group1 <- plot_data$plotting_data %>%
    dplyr::filter(.data[[moderator]] == moderator_levels[1])

  observed_counts_group1 <- plot_data$observed_counts %>%
    dplyr::filter(.data[[moderator]] == moderator_levels[1])

  pred_summary_group2 <- plot_data$plotting_data %>%
    dplyr::filter(.data[[moderator]] == moderator_levels[2])

  observed_counts_group2 <- plot_data$observed_counts %>%
    dplyr::filter(.data[[moderator]] == moderator_levels[2])

  group1_sunflower <- do.call(plotSunflower, c(
    list(
      pred_summary = pred_summary_group1,
      observed_counts = observed_counts_group1,
      x_var = x_var,
      join_vars = join_vars,
      colors = colors,
      category_labels = category_labels,
      title = group_labels[1],
      y_label = "Predicted Probability",
      x_label = "",
      legend_title = "Response\nCategory"
    ),
    sunflower_params
  ))

  group2_sunflower <- do.call(plotSunflower, c(
    list(
      pred_summary = pred_summary_group2,
      observed_counts = observed_counts_group2,
      x_var = x_var,
      join_vars = join_vars,
      colors = colors,
      category_labels = category_labels,
      title = group_labels[2],
      y_label = "Predicted Probability",
      x_label = "",
      legend_title = "Response\nCategory"
    ),
    sunflower_params
  ))

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

  group1_no_legend <- group1_sunflower +
    ggplot2::theme(legend.position = "none")

  combined_plot <- group1_no_legend + group1_margins +
    group2_sunflower + group2_margins +
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

  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, combined_plot, width = width, height = height)
  }

  return(combined_plot)
}
