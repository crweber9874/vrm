#' Plot Predicted Effects with Sunflower Distribution
#'
#' Creates a visualization showing predicted effects or probabilities
#' with sunflower plots showing the distribution of observations and
#' connector lines to the axis.
#'
#' @param plot_data A list containing:
#'   - plotting_data: data frame with mean_prob, lower_ci, upper_ci, .category, etc.
#'   - observed_counts: data frame with category, n, and matching variables
#' @param join_vars Character vector of variable names to use for joining.
#'   Default: c("category", "prepost")
#' @param y_var Character. Name of the y-axis variable (point estimate).
#'   Default: "mean_prob"
#' @param x_var Character. Name of the x-axis variable. Default: "category"
#' @param color_var Character. Name of variable for coloring dots.
#'   Default: "prepost"
#' @param facet_var Character. Optional name of variable for faceting.
#'   Default: NULL
#' @param category_levels Character vector. Underlying levels of category variable.
#'   Default: c("1", "2", "3", "4", "5")
#' @param category_labels Character vector. Labels for categories.
#'   Default: c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree",
#'              "Agree", "Strongly Agree")
#' @param color_levels Character vector. Underlying levels of color variable.
#'   Default: c(0, 1)
#' @param color_labels Character vector. Labels for color variable.
#'   Default: c("Pre", "Post")
#' @param color_values Character vector. Colors for each level.
#'   Default: c("grey", "black")
#' @param facet_levels Character vector. Underlying levels of facet variable.
#'   Default: c("0", "1")
#' @param facet_labels Character vector. Labels for facet variable.
#'   Default: c("Group 0", "Group 1")
#' @param title Main title for the plot. Default: "Predicted Effects"
#' @param x_label Label for the x-axis. Default: ""
#' @param y_label Label for the y-axis. Default: "Predicted Probability"
#' @param mean_point_size Size of the mean dots. Default: 3
#' @param mean_point_alpha Transparency of the mean dots. Default: 0.8
#' @param sunflower_size Size of sunflower points. Default: 0.01
#' @param sunflower_alpha Transparency of sunflower points. Default: 0.25
#' @param y_limits Numeric vector of length 2 specifying y-axis limits.
#'   Default: c(0, 1)
#' @param dodge_width Width for position dodging. Default: 0.65
#' @param sunflower_density Density parameter for sunflower positioning. Default: 60
#' @param sunflower_aspect_ratio Aspect ratio for sunflower positioning. Default: 7
#' @param coord_flip Logical. Whether to flip coordinates. Default: TRUE
#' @param connector_lines Logical. Whether to draw lines from axis to points. Default: TRUE
#' @param connector_linewidth Width of connector lines. Default: 0.5
#' @param connector_alpha Transparency of connector lines. Default: 0.5
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' # Basic usage with sunflowers
#' plotSunflowerDots(plot_burn)
#'
#' # Filter to Trump voters only
#' plotSunflowerDots(
#'   list(
#'     plotting_data = plot_burn$plotting_data |> filter(vote_trump == 1),
#'     observed_counts = plot_burn$observed_counts |> filter(vote_trump == 1)
#'   ),
#'   join_vars = c("category", "prepost", "vote_trump")
#' )
#'
#' # With faceting
#' plotSunflowerDots(plot_burn,
#'                   facet_var = "vote_trump",
#'                   facet_levels = c("0", "1"),
#'                   facet_labels = c("Biden Voters", "Trump Voters"))
#'
#' # Without connector lines
#' plotSunflowerDots(plot_burn, connector_lines = FALSE)
#'
#' # Custom sunflower density
#' plotSunflowerDots(plot_burn,
#'                   sunflower_density = 100,
#'                   sunflower_size = 0.02)
#' }
#'
#' @export
plotSunflowerDots <- function(plot_data,
                              join_vars = c("category", "prepost"),
                              y_var = "mean_prob",
                              x_var = "category",
                              color_var = "prepost",
                              facet_var = NULL,
                              category_levels = c("1", "2", "3", "4", "5"),
                              category_labels = c("Strongly Disagree", "Disagree",
                                                  "Neither Agree nor Disagree",
                                                  "Agree", "Strongly Agree"),
                              color_levels = c(0, 1),
                              color_labels = c("Pre", "Post"),
                              color_values = c("grey", "black"),
                              facet_levels = c("0", "1"),
                              facet_labels = c("Group 0", "Group 1"),
                              title = "Predicted Effects",
                              x_label = "",
                              y_label = "Predicted Probability",
                              mean_point_size = 3,
                              mean_point_alpha = 0.8,
                              sunflower_size = 0.01,
                              sunflower_alpha = 0.25,
                              y_limits = c(0, 1),
                              dodge_width = 0.65,
                              sunflower_density = 60,
                              sunflower_aspect_ratio = 7,
                              coord_flip = TRUE,
                              connector_lines = TRUE,
                              connector_linewidth = 0.5,
                              connector_alpha = 0.5) {

  require(ggplot2)
  require(dplyr)
  require(tidyr)
  require(rlang)
  require(vayr)

  # Extract data from list
  if (is.list(plot_data) && "plotting_data" %in% names(plot_data)) {
    plotting_data <- plot_data$plotting_data
    observed_counts <- plot_data$observed_counts
  } else {
    stop("plot_data must be a list with 'plotting_data' and 'observed_counts' elements")
  }

  # Input validation
  if (!y_var %in% names(plotting_data)) {
    stop("y_var '", y_var, "' not found in plotting_data")
  }

  if (!x_var %in% names(plotting_data)) {
    stop("x_var '", x_var, "' not found in plotting_data")
  }

  if (!color_var %in% names(plotting_data)) {
    stop("color_var '", color_var, "' not found in plotting_data")
  }

  if (!is.null(facet_var) && !facet_var %in% names(plotting_data)) {
    stop("facet_var '", facet_var, "' not found in plotting_data")
  }

  # Prepare plotting data with factor levels
  plot_dat_prepared <- plotting_data %>%
    dplyr::mutate(
      !!sym(x_var) := factor(.data[[x_var]],
                             levels = category_levels,
                             labels = category_labels),
      !!sym(color_var) := factor(.data[[color_var]],
                                 levels = color_levels,
                                 labels = color_labels)
    )

  # Add facet variable preparation if needed
  if (!is.null(facet_var)) {
    plot_dat_prepared <- plot_dat_prepared %>%
      dplyr::mutate(!!sym(facet_var) := factor(.data[[facet_var]],
                                               levels = facet_levels,
                                               labels = facet_labels))
  }

  # Prepare sunflower data by joining and expanding
  sunflower_data <- observed_counts %>%
    dplyr::left_join(
      plotting_data %>% dplyr::select(dplyr::all_of(c(join_vars, y_var))),
      by = join_vars
    ) %>%
    tidyr::uncount(n) %>%
    dplyr::filter(!is.na(.data[[y_var]])) %>%
    dplyr::mutate(
      !!sym(x_var) := factor(.data[[x_var]],
                             levels = category_levels,
                             labels = category_labels),
      !!sym(color_var) := factor(.data[[color_var]],
                                 levels = color_levels,
                                 labels = color_labels)
    )

  # Add facet variable to sunflower data if needed
  if (!is.null(facet_var)) {
    sunflower_data <- sunflower_data %>%
      dplyr::mutate(!!sym(facet_var) := factor(.data[[facet_var]],
                                               levels = facet_levels,
                                               labels = facet_labels))
  }

  # Create base plot
  plot <- ggplot2::ggplot(plot_dat_prepared,
                          ggplot2::aes(x = .data[[x_var]],
                                       y = .data[[y_var]],
                                       color = .data[[color_var]]))

  # Add connector lines if requested
  if (connector_lines) {
    plot <- plot +
      ggplot2::geom_linerange(ggplot2::aes(ymin = 0, ymax = .data[[y_var]]),
                              linewidth = connector_linewidth,
                              alpha = connector_alpha,
                              position = ggplot2::position_dodge(width = dodge_width))
  }

  # Add sunflower points
  plot <- plot +
    ggplot2::geom_point(data = sunflower_data,
                        ggplot2::aes(x = .data[[x_var]],
                                     y = .data[[y_var]],
                                     color = .data[[color_var]]),
                        position = suppressMessages(
                          vayr::position_sunflowerdodge(width = dodge_width,
                                                        density = sunflower_density,
                                                        aspect_ratio = sunflower_aspect_ratio)),
                        size = sunflower_size,
                        alpha = sunflower_alpha,
                        inherit.aes = FALSE)

  # Add mean points on top
  plot <- plot +
    ggplot2::geom_point(size = mean_point_size,
                        alpha = mean_point_alpha,
                        position = ggplot2::position_dodge(width = dodge_width))

  # Add faceting if requested
  if (!is.null(facet_var)) {
    plot <- plot + ggplot2::facet_wrap(~.data[[facet_var]], nrow = 2)
  }

  # Add labels and theme
  plot <- plot +
    ggplot2::ggtitle(title) +
    ggplot2::scale_y_continuous(y_label, limits = y_limits) +
    ggplot2::scale_x_discrete(x_label) +
    ggplot2::scale_colour_manual(
      name = "",
      values = color_values,
      labels = color_labels
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(size = 8),
      axis.title = ggplot2::element_text(size = 10),
      plot.title = ggplot2::element_text(size = 12, face = "bold")
    )

  # Flip coordinates if requested
  if (coord_flip) {
    plot <- plot + ggplot2::coord_flip()
  }

  return(plot)
}
