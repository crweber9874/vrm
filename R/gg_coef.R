#' Coefficient / Effect Plot with Interval Whiskers
#'
#' Dot-and-whisker plot of point estimates and intervals: a reference line,
#' optional color dodging across groups (e.g., conditions, voter groups, model
#' specifications), and terms listed top to bottom in data order.
#' Accepts either a summary data frame or a `brmsfit` (fixed effects via
#' [tidy_fixed_effects()]).
#'
#' @param x A data frame of summaries, or a `brmsfit`.
#' @param term Character. Column labeling rows. Default `"term"`.
#' @param estimate,lower,upper Character. Columns for the point estimate and
#'   interval bounds. If `NULL`, detected from common names
#'   (`estimate`/`mean`/`effect`, `conf.low`/`lower`/`lower_ci`,
#'   `conf.high`/`upper`/`upper_ci`).
#' @param color Character or `NULL`. Grouping column mapped to color and dodged.
#' @param facet Character or `NULL`. Column for `facet_wrap()`.
#' @param ref Numeric or `NULL`. Reference line position. Default `0`.
#' @param drop_intercept Logical. Drop terms matching `"Intercept"`. Default `TRUE`.
#' @param labels Optional named character vector relabeling `term` values
#'   (names = values in data). Also fixes display order.
#' @param dodge Dodge width when `color` is supplied. Default `0.5`.
#' @param xlab,title Axis label for the estimate and plot title.
#' @param ... Passed to `ggplot2::geom_pointrange()`.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' est <- data.frame(
#'   term  = rep(c("Fraud", "Norms"), 2),
#'   group = rep(c("Biden", "Trump"), each = 2),
#'   mean  = c(0.10, 0.02, -0.05, 0.08),
#'   lower = c(0.02, -0.06, -0.12, 0.01),
#'   upper = c(0.18, 0.10, 0.02, 0.15)
#' )
#' gg_coef(est, color = "group", xlab = "Marginal effect")
#'
#' \dontrun{
#' gg_coef(brms_fit_object)
#' }
gg_coef <- function(x, term = "term", estimate = NULL, lower = NULL, upper = NULL,
                    color = NULL, facet = NULL, ref = 0, drop_intercept = TRUE,
                    labels = NULL, dodge = 0.5, xlab = "Estimate", title = NULL, ...) {
  if (inherits(x, "brmsfit")) {
    if (!requireNamespace("brms", quietly = TRUE)) {
      stop("Package 'brms' is required for brmsfit input.", call. = FALSE)
    }
    x <- tidy_fixed_effects(x)
  }
  if (!is.data.frame(x)) stop("`x` must be a data frame or a brmsfit.", call. = FALSE)
  if (nrow(x) == 0) stop("`x` has no rows.", call. = FALSE)

  detect <- function(arg, candidates, role) {
    if (!is.null(arg)) return(arg)
    hit <- intersect(candidates, names(x))
    if (length(hit) == 0) {
      stop("Could not detect the ", role, " column; supply it explicitly. Columns: ",
           paste(names(x), collapse = ", "), call. = FALSE)
    }
    hit[1]
  }
  estimate <- detect(estimate, c("estimate", "mean", "effect", "Estimate", ".epred"), "estimate")
  lower    <- detect(lower, c("conf.low", "lower", "lower_ci", ".lower", "Q2.5"), "lower")
  upper    <- detect(upper, c("conf.high", "upper", "upper_ci", ".upper", "Q97.5"), "upper")
  .check_cols(x, c(term, estimate, lower, upper, color, facet))

  num <- c(estimate, lower, upper)
  non_num <- num[!purrr::map_lgl(x[num], is.numeric)]
  if (length(non_num)) stop("Non-numeric columns: ", paste(non_num, collapse = ", "), call. = FALSE)
  if (any(x[[lower]] > x[[upper]], na.rm = TRUE)) {
    stop("Some `lower` values exceed `upper`; check column order.", call. = FALSE)
  }

  if (drop_intercept) x <- dplyr::filter(x, !grepl("Intercept", .data[[term]]))
  if (nrow(x) == 0) stop("No rows remain after dropping intercepts.", call. = FALSE)

  if (!is.null(labels)) {
    unmatched <- setdiff(unique(as.character(x[[term]])), names(labels))
    if (length(unmatched)) {
      stop("`labels` is missing term(s): ", paste(unmatched, collapse = ", "), call. = FALSE)
    }
    x[[term]] <- factor(as.character(x[[term]]), levels = names(labels), labels = labels)
  } else if (!is.factor(x[[term]])) {
    x[[term]] <- factor(x[[term]], levels = unique(x[[term]]))
  }
  x[[term]] <- .rev_levels(x[[term]])

  pos <- if (is.null(color)) "identity" else ggplot2::position_dodge(width = dodge)
  aes_main <- if (is.null(color)) {
    ggplot2::aes(x = .data[[term]], y = .data[[estimate]],
                 ymin = .data[[lower]], ymax = .data[[upper]])
  } else {
    ggplot2::aes(x = .data[[term]], y = .data[[estimate]],
                 ymin = .data[[lower]], ymax = .data[[upper]],
                 color = .data[[color]])
  }

  p <- ggplot2::ggplot(x, aes_main)
  if (!is.null(ref)) {
    p <- p + ggplot2::geom_hline(yintercept = ref, linetype = "dashed", color = "grey60")
  }
  p <- p +
    ggplot2::geom_pointrange(position = pos, ...) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = xlab, color = NULL, title = title) +
    theme_vrm()

  if (!is.null(facet)) p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet]]))
  p
}

.rev_levels <- function(f) factor(f, levels = rev(levels(f)))
