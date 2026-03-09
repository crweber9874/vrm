#' @title ggMargins_categorical
#' @description Generate plot of point estimates from BRMS object
#'
#' This function generates a `ggplot2` plot of posterior point estimates and credible intervals
#' from a `brms` model, specifically designed for cumulative or categorical outcomes.
#' The data come from the 2020 Western States Survey.
#'
#' @param data Dataframe containing posterior predictions, typically from `electoralContestation::posterior_me`
#'   or similar prediction functions. It must include columns named `.category` (for the
#'   response categories), `mean`, `lower`, `upper` (for credible intervals), and the column
#'   specified by `mvar` (the moderator variable). Defaults to `plot_dat`.
#' @param size_x Numeric value. This controls the font size for the axis text. Defaults to `10`.
#' @param size_title Numeric value. Font size for the plot title and axis titles. Defaults to `12`.
#' @param title Character string. Main title of the the plot. Defaults to `"Marginal Effect"`.
#' @param ytitle Character string. Label for the Y-axis (which becomes the X-axis after `coord_flip()`). Defaults to `"Change in Probability from Pre to Post"`.
#' @param ylimits Numeric vector of length 2. Lower and upper limits for the Y-axis (which becomes the X-axis after `coord_flip()`). Defaults to `c(-0.25, 0.25)`.
#' @param mvar Character string. Name of the moderator variable column in the `data` dataframe, used for grouping and coloring the points. Defaults to `"presvote_trump_2020"`.
#' @param point_size Numeric. Size of the data points in `geom_point()`. Defaults to `3`.
#' @param point_opacity Numeric. Transparency (alpha) of the data points. Defaults to `0.5`.
#' @param bar_opacity Numeric. Transparency (alpha) of the error bars. Defaults to `0.5`.
#' @param point_color_values Character vector. Colors used for the levels of the moderator variable (`mvar`). Defaults to `c("#E63946", "#457B9D")`.
#' @param point_distance Numeric. Dodge width for `geom_point()` and `geom_errorbar()` to horizontally separate points and error bars for different groups on the categorical axis. Defaults to `0.5`.
#' @param ... Additional arguments passed to `ggplot2::theme()` for further customization of plot elements.
#' @export

ggMargins_categories <- function(data = plot_dat,
                                  size_x = 10,
                                  size_title = 12,
                                  title = "Marginal Effect",
                                  ytitle = "Change in Probability",
                                  ylimits = c(-0.25, 0.25),
                                  mvar = "presvote_trump_2020",
                                  point_size = 3,
                                  point_opacity = 0.5,
                                  bar_opacity = 0.5,
                                  point_color_values = c("#E63946", "#457B9D"),
                                  point_distance = 0.5,
                                  ...) {

  if (!(".category" %in% names(data))) {
    warning("This function is only for cumulative and categorical models and requires a '.category' column in the input data.")
    return(NULL)
  }
  else{
    plot <-
      ggplot2::ggplot(data)  +
      ggplot2::aes(
        x = as.factor(.category),
        y = mean,
        ymin = lower,
        ymax = upper,
        group = as.factor(!!sym(mvar)),
        color = as.factor(!!sym(mvar))
      ) +
      ggplot2::geom_point(size = point_size, position = position_dodge(width = point_distance), alpha = point_opacity) +
      ggplot2::geom_errorbar(width = 0.15,  position = position_dodge(width = point_distance),  alpha = bar_opacity, colour = "black") +
      ggplot2::ggtitle(title) +
      ggplot2::scale_x_discrete("") +
      ggplot2::scale_y_continuous(ytitle, limits = ylimits) +
      ggplot2::geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +    # Cleaner base theme
      ggplot2::scale_color_manual("", values = point_color_values) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = size_title, hjust = 0, vjust = 0, colour = "#3C3C3C"),
        axis.text = ggplot2::element_text(size = size_x, colour = "#535353"),
        axis.title = ggplot2::element_text(size = size_title, colour = "#535353"),
        axis.title.y = ggplot2::element_text(vjust = 1.5),
        axis.ticks = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_text(size = size_x),
        panel.grid.major = ggplot2::element_line(colour = "#D0D0D0", linewidth = 0.25), # Changed size to linewidth for ggplot2 3.4.0+
        legend.position = "bottom",
        ...
      )
    return(plot)
  }

}
