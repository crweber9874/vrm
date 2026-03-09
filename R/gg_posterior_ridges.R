#' Ridgeplot of Posterior Predictions
#'
#' Creates a ridgeplot (joy plot) from posterior prediction draws using
#' \pkg{ggridges}.
#' Each ridge represents a level of \code{ridge_var} (e.g., contestation
#' items), and the x-axis shows the posterior predictive distribution.
#' Ridges are filled by \code{fill_var} (e.g., pre/post election) and
#' optionally faceted by a conditioning variable.
#'
#' @param draws A \code{tibble} of posterior draws, typically output from
#'   \code{\link{posterior_draws_grid}} with an added grouping column
#'   (e.g., \code{item}).
#' @param ridge_var Character. Column mapped to the y-axis ridges
#'   (e.g., \code{"item"}).
#' @param fill_var Character. Column mapped to fill colour
#'   (e.g., \code{"prepost"}).
#' @param facet_var Character or \code{NULL}. Column for
#'   \code{facet_wrap()}. \code{NULL} produces a single panel.
#' @param category Character or \code{NULL}. For ordinal / multinomial
#'   models, keep only draws where \code{.category == category}
#'   (e.g., \code{"5"} for Pr(y = 5)).
#'   Ignored when \code{.category} is absent (linear models).
#' @param ridge_labels Named character vector mapping raw levels of
#'   \code{ridge_var} to display labels. Names = data values,
#'   values = labels.
#' @param fill_labels Named character vector for \code{fill_var} labels.
#' @param facet_labels Named character vector for \code{facet_var} labels.
#' @param viridis_option Character. A viridis palette name
#'   (\code{"viridis"}, \code{"magma"}, \code{"plasma"}, \code{"inferno"},
#'   \code{"cividis"}, \code{"mako"}, \code{"rocket"}, \code{"turbo"}).
#' @param title Character. Plot title.
#' @param xlab Character. X-axis label.
#' @param alpha Numeric in \code{[0, 1]}. Fill transparency.
#' @param scale Numeric. \pkg{ggridges} \code{scale} parameter controlling
#'   vertical overlap between ridges.
#' @param quantile_lines Logical. If \code{TRUE}, draw median and
#'   50\% quantile lines inside each ridge.
#' @param ... Additional arguments forwarded to
#'   \code{ggridges::geom_density_ridges()}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' draws <- bind_rows(lapply(items, function(it) {
#'   posterior_draws_grid(
#'     models[[paste0(it, "_linear")]],
#'     xvar = "prepost", mvar = "vote_trump"
#'   ) %>% mutate(item = it)
#' }))
#'
#' gg_posterior_ridges(
#'   draws,
#'   ridge_var    = "item",
#'   fill_var     = "prepost",
#'   facet_var    = "vote_trump",
#'   fill_labels  = c("0" = "Pre-Election", "1" = "Post-Election"),
#'   facet_labels = c("0" = "Biden Voter", "1" = "Trump Voter"),
#'   title        = "Predicted Mean Support (Linear)"
#' )
#' }
#'
#' @export
gg_posterior_ridges <- function(draws,
                               ridge_var,
                               fill_var,
                               facet_var = NULL,
                               category  = NULL,
                               ridge_labels  = NULL,
                               fill_labels   = NULL,
                               facet_labels  = NULL,
                               viridis_option = "viridis",
                               title = NULL,
                               xlab  = "More Support \u2192",
                               alpha = 0.5,
                               scale = 0.9,
                               quantile_lines = TRUE,
                               ...) {
  # Filter to a specific response category (ordinal / multinomial)
  if (!is.null(category) && ".category" %in% names(draws)) {
    draws <- dplyr::filter(draws, .category == category)
  }

  df <- dplyr::ungroup(draws)

  # Relabel factors --------------------------------------------------------
  relabel <- function(data, var, labels) {
    if (is.null(labels)) return(data)
    dplyr::mutate(data, !!var := factor(
      as.character(.data[[var]]), levels = names(labels), labels = labels
    ))
  }

  df <- relabel(df, ridge_var, ridge_labels)
  df <- relabel(df, fill_var,  fill_labels)
  if (!is.null(facet_var)) df <- relabel(df, facet_var, facet_labels)

  # Build plot -------------------------------------------------------------
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x    = .epred,
      y    = .data[[ridge_var]],
      fill = .data[[fill_var]]
    )
  ) +
    ggridges::geom_density_ridges(
      alpha = alpha, scale = scale,
      quantile_lines = quantile_lines, ...
    ) +
    ggplot2::scale_fill_viridis_d(option = viridis_option, end = 0.8, name = NULL) +
    ggplot2::labs(title = title, x = xlab, y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position  = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(size = 11),
      axis.title.x     = ggplot2::element_text(hjust = 1)
    )

  if (!is.null(facet_var)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]]))
  }

  p
}
