#' Plot Predicted Effects as Simple Dots
#'
#' Creates a minimal visualization showing predicted effects or probabilities
#' as dots, with optional grouping by condition.
#'
#' @param plot_dat A data frame with prediction summaries containing:
#'   - mean, lower, upper: point estimates and intervals
#'   - .category: for ordinal/categorical outcomes
#'   - grouping variables (e.g., prepost, presvote_trump_2020)
#' @param y_var Character. Name of the y-axis variable (point estimate).
#'   Default: "mean"
#' @param x_var Character. Name of the x-axis variable. Default: ".category"
#' @param color_var Character. Name of variable for coloring dots. Set to NULL for no color grouping.
#'   Default: "prepost"
#' @param facet_var Character. Optional name of variable for faceting.
#'   Default: NULL
#' @param category_levels Character vector. Underlying levels of category variable.
#'   Default: c("1", "2", "3", "4", "5")
#' @param category_labels Character vector. Labels for categories.
#'   Default: c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree",
#'              "Agree", "Strongly Agree")
#' @param color_levels Character vector. Underlying levels of color variable.
#'   Default: c("pre", "post")
#' @param color_labels Character vector. Labels for color variable.
#'   Default: c("Pre-Survey", "Post-Survey")
#' @param color_values Character vector. Colors for each level.
#'   Default: c("grey", "black")
#' @param facet_levels Character vector. Underlying levels of facet variable.
#'   Default: c("0", "1")
#' @param facet_labels Character vector. Labels for facet variable.
#'   Default: c("Group 0", "Group 1")
#' @param title Main title for the plot. Default: "Predicted Effects"
#' @param x_label Label for the x-axis. Default: ""
#' @param y_label Label for the y-axis. Default: "Posterior Mean Estimate"
#' @param point_size Size of the dots. Default: 3
#' @param point_alpha Transparency of the dots. Default: 0.8
#' @param y_limits Numeric vector of length 2 specifying y-axis limits.
#'   Default: NULL
#' @param dodge_width Width for position dodging. Default: 0.6
#' @param coord_flip Logical. Whether to flip coordinates. Default: TRUE
#' @param connector_lines Logical. Whether to draw lines from axis to points. Default: TRUE
#' @param connector_linewidth Width of connector lines. Default: 1
#' @param connector_alpha Transparency of connector lines. Default: 0.6
#' @param show_errorbar Logical. Whether to show error bars for confidence intervals. Default: FALSE
#' @param errorbar_width Width of error bars. Default: 0.2
#' @param errorbar_linewidth Line width of error bars. Default: 0.8
#' @param ci_lower Name of column with lower CI bound. Default: NULL (auto-detects "lower_ci" or "lower")
#' @param ci_upper Name of column with upper CI bound. Default: NULL (auto-detects "upper_ci" or "upper")
#' @param reference_line Numeric. Optional y-value for reference line (e.g., 0 for marginal effects). Default: NULL
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' # Basic usage with default settings (includes connector lines)
#' plotDots(plot_dat)
#'
#' # Filter to Trump voters only, show pre/post comparison
#' plotDots(plot_burn$plotting_data |> filter(vote_trump == 1),
#'          y_var = "mean_prob",
#'          color_var = "prepost",
#'          color_levels = c(0, 1),
#'          color_labels = c("Pre", "Post"),
#'          color_values = c("grey", "black"))
#'
#' # Marginal effects with error bars (auto-detects "lower" and "upper" columns)
#' plotDots(me_data,
#'          y_var = "mean",
#'          x_var = ".category",
#'          color_var = NULL,
#'          show_errorbar = TRUE,
#'          reference_line = 0,
#'          y_limits = c(-0.3, 0.3),
#'          y_label = "Marginal Effect")
#'
#' # Marginal effects with custom CI columns
#' plotDots(me_data,
#'          y_var = "mean_effect",
#'          x_var = "category",
#'          color_var = NULL,
#'          show_errorbar = TRUE,
#'          ci_lower = "lower_ci",
#'          ci_upper = "upper_ci",
#'          reference_line = 0,
#'          y_limits = c(-0.3, 0.3))
#'
#' # With faceting by Trump vote
#' plotDots(plot_dat,
#'          facet_var = "presvote_trump_2020",
#'          facet_levels = c("0", "1"),
#'          facet_labels = c("Biden Voters", "Trump Voters"))
#'
#' # Without connector lines
#' plotDots(plot_dat, connector_lines = FALSE)
#'
#' # Custom line styling
#' plotDots(plot_dat,
#'          connector_linewidth = 1.5,
#'          connector_alpha = 0.8)
#' }
#'
#' @export
plotDots <- function(plot_dat,
                     y_var = "mean",
                     x_var = ".category",
                     color_var = "prepost",
                     facet_var = NULL,
                     category_levels = c("1", "2", "3", "4", "5"),
                     category_labels = c("Strongly Disagree", "Disagree",
                                         "Neither Agree nor Disagree",
                                         "Agree", "Strongly Agree"),
                     color_levels = c("pre", "post"),
                     color_labels = c("Pre-Survey", "Post-Survey"),
                     color_values = c("grey", "black"),
                     facet_levels = c("0", "1"),
                     facet_labels = c("Group 0", "Group 1"),
                     title = "Predicted Effects",
                     x_label = "",
                     y_label = "Posterior Mean Estimate",
                     point_size = 3,
                     point_alpha = 0.8,
                     y_limits = NULL,
                     dodge_width = 0.6,
                     coord_flip = TRUE,
                     connector_lines = TRUE,
                     connector_linewidth = 1,
                     connector_alpha = 0.6,
                     show_errorbar = FALSE,
                     errorbar_width = 0.2,
                     errorbar_linewidth = 0.8,
                     ci_lower = NULL,
                     ci_upper = NULL,
                     reference_line = NULL) {

  require(ggplot2)
  require(dplyr)
  require(rlang)

  # Auto-detect CI column names if show_errorbar is TRUE
  if (show_errorbar) {
    if (is.null(ci_lower)) {
      if ("lower_ci" %in% names(plot_dat)) {
        ci_lower <- "lower_ci"
      } else if ("lower" %in% names(plot_dat)) {
        ci_lower <- "lower"
      } else {
        warning("show_errorbar is TRUE but could not auto-detect lower CI column.")
        show_errorbar <- FALSE
      }
    }

    if (is.null(ci_upper)) {
      if ("upper_ci" %in% names(plot_dat)) {
        ci_upper <- "upper_ci"
      } else if ("upper" %in% names(plot_dat)) {
        ci_upper <- "upper"
      } else {
        warning("show_errorbar is TRUE but could not auto-detect upper CI column.")
        show_errorbar <- FALSE
      }
    }
  }

  # Input validation
  if (!y_var %in% names(plot_dat)) {
    stop("y_var '", y_var, "' not found in plot_dat")
  }

  if (!x_var %in% names(plot_dat)) {
    stop("x_var '", x_var, "' not found in plot_dat")
  }

  if (!is.null(color_var) && !color_var %in% names(plot_dat)) {
    stop("color_var '", color_var, "' not found in plot_dat")
  }

  if (!is.null(facet_var) && !facet_var %in% names(plot_dat)) {
    stop("facet_var '", facet_var, "' not found in plot_dat")
  }

  # Prepare data with factor levels
  plot_dat_prepared <- plot_dat %>%
    mutate(
      !!sym(x_var) := factor(.data[[x_var]],
                             levels = category_levels,
                             labels = category_labels)
    )

  # Add color variable if provided
  if (!is.null(color_var)) {
    plot_dat_prepared <- plot_dat_prepared %>%
      mutate(!!sym(color_var) := factor(.data[[color_var]],
                                        levels = color_levels,
                                        labels = color_labels))
  }

  # Add facet variable preparation if needed
  if (!is.null(facet_var)) {
    plot_dat_prepared <- plot_dat_prepared %>%
      mutate(!!sym(facet_var) := factor(.data[[facet_var]],
                                        levels = facet_levels,
                                        labels = facet_labels))
  }

  # Create base plot with optional reference line
  if (is.null(color_var)) {
    plot <- ggplot(plot_dat_prepared,
                   aes(x = .data[[x_var]],
                       y = .data[[y_var]]))
  } else {
    plot <- ggplot(plot_dat_prepared,
                   aes(x = .data[[x_var]],
                       y = .data[[y_var]],
                       color = .data[[color_var]]))
  }

  # Add reference line if specified
  if (!is.null(reference_line)) {
    plot <- plot +
      geom_hline(yintercept = reference_line,
                 linetype = "dashed",
                 color = "darkgrey",
                 linewidth = 0.5)
  }

  # Add connector lines if requested
  if (connector_lines) {
    if (is.null(color_var)) {
      plot <- plot +
        geom_linerange(aes(ymin = 0, ymax = .data[[y_var]]),
                       linewidth = connector_linewidth,
                       alpha = connector_alpha)
    } else {
      plot <- plot +
        geom_linerange(aes(ymin = 0, ymax = .data[[y_var]]),
                       position = position_dodge(width = dodge_width),
                       linewidth = connector_linewidth,
                       alpha = connector_alpha)
    }
  }

  # Add error bars if requested
  if (show_errorbar && !is.null(ci_lower) && !is.null(ci_upper)) {
    if (ci_lower %in% names(plot_dat_prepared) && ci_upper %in% names(plot_dat_prepared)) {
      if (is.null(color_var)) {
        plot <- plot +
          geom_errorbar(aes(ymin = .data[[ci_lower]], ymax = .data[[ci_upper]]),
                        width = errorbar_width,
                        linewidth = errorbar_linewidth)
      } else {
        plot <- plot +
          geom_errorbar(aes(ymin = .data[[ci_lower]], ymax = .data[[ci_upper]]),
                        width = errorbar_width,
                        linewidth = errorbar_linewidth,
                        position = position_dodge(width = dodge_width))
      }
    }
  }

  # Add points
  if (is.null(color_var)) {
    plot <- plot +
      geom_point(size = point_size,
                 alpha = point_alpha)
  } else {
    plot <- plot +
      geom_point(size = point_size,
                 alpha = point_alpha,
                 position = position_dodge(width = dodge_width))
  }

  # Add faceting if requested
  if (!is.null(facet_var)) {
    plot <- plot + facet_wrap(~.data[[facet_var]], nrow = 2)
  }

  # Add labels and theme
  plot <- plot +
    ggtitle(title) +
    scale_x_discrete(x_label) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.major.x = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      text = element_text(size = 10),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 10),
      plot.title = element_text(size = 12, face = "bold")
    )

  # Add y-axis scale
  if (!is.null(y_limits)) {
    plot <- plot + scale_y_continuous(y_label, limits = y_limits)
  } else {
    plot <- plot + scale_y_continuous(y_label)
  }

  # Add color scale if color_var is provided
  if (!is.null(color_var)) {
    plot <- plot +
      scale_colour_manual(
        name = "",
        values = color_values,
        labels = color_labels
      )
  }

  # Flip coordinates if requested
  if (coord_flip) {
    plot <- plot + coord_flip()
  }

  return(plot)
}
