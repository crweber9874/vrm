#' Forest Plot of Posterior Predictive Differences
#'
#' Creates a point-and-interval plot of posterior predictive differences
#' (e.g., post minus pre), with flexible aesthetics for colour, faceting,
#' and axis mapping.
#'
#' @param data A data frame with columns for the item variable, a
#'   colour / group variable, and \code{mean}, \code{lower}, \code{upper}.
#' @param x_var Character. Column mapped to the y-axis (items). Displayed
#'   vertically via \code{coord_flip()}.
#' @param color_var Character. Column mapped to point/interval colour
#'   (e.g., vote choice).
#' @param facet_var Character or \code{NULL}. Column for
#'   \code{facet_wrap()}.
#' @param x_labels Named character vector. Display labels for
#'   \code{x_var} levels.
#' @param color_labels Named character vector. Display labels for
#'   \code{color_var} levels.
#' @param facet_labels Named character vector. Display labels for
#'   \code{facet_var} levels. Passed via \code{labeller}.
#' @param viridis_option Character. A viridis palette name (default
#'   \code{"viridis"}).
#' @param title Character. Plot title.
#' @param ylab Character. Label for the effect-size axis.
#' @param point_size Numeric. Size of point estimates.
#' @param dodge_width Numeric. Horizontal dodge for overlapping groups.
#' @param ref_line Numeric. Position of the reference line (default 0).
#' @param ... Additional arguments passed to \code{ggplot2::theme()}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' gg_pred_diff(
#'   diff_df,
#'   x_var       = "item",
#'   color_var   = "vote_trump",
#'   facet_var   = "model_type",
#'   color_labels = c("0" = "Biden Voter", "1" = "Trump Voter"),
#'   title = "Posterior Predictive Difference (Post - Pre)"
#' )
#' }
#'
#' @export
gg_pred_diff <- function(data,
                         x_var,
                         color_var,
                         facet_var     = NULL,
                         x_labels     = NULL,
                         color_labels = NULL,
                         facet_labels = NULL,
                         viridis_option = "viridis",
                         title   = NULL,
                         ylab    = "Predicted Difference (Post \u2212 Pre)",
                         point_size   = 2.5,
                         dodge_width  = 0.5,
                         ref_line     = 0,
                         ...) {

  df <- data

  # Relabel factors --------------------------------------------------------
  relabel <- function(d, var, labels) {
    if (is.null(labels)) return(d)
    dplyr::mutate(d, !!var := factor(
      as.character(.data[[var]]), levels = names(labels), labels = labels
    ))
  }

  df <- relabel(df, x_var,     x_labels)
  df <- relabel(df, color_var, color_labels)
  if (!is.null(facet_var) && !is.null(facet_labels)) {
    df <- relabel(df, facet_var, facet_labels)
  }

  pos <- ggplot2::position_dodge(width = dodge_width)

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x     = .data[[x_var]],
      y     = mean,
      ymin  = lower,
      ymax  = upper,
      color = .data[[color_var]]
    )
  ) +
    ggplot2::geom_hline(
      yintercept = ref_line, linetype = "dashed", color = "grey50"
    ) +
    ggplot2::geom_pointrange(size = point_size / 3, position = pos) +
    ggplot2::coord_flip() +
    ggplot2::scale_color_viridis_d(option = viridis_option, end = 0.8, name = NULL) +
    ggplot2::labs(title = title, x = NULL, y = ylab) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position  = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(size = 11),
      ...
    )

  if (!is.null(facet_var)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]))
  }

  p
}
