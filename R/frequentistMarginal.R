#' Calculate Marginal Effects from Predicted Probabilities
#'
#' Computes the marginal effect (difference in predicted probabilities) when
#' changing a focal variable, optionally moderated by another variable.
#'
#' @param predictions_long Data frame with predictions in long format (from
#'   predict_ordinal_probs or predict_logit_probs), must include 'draw' column
#' @param focal_var Character. Name of the focal variable for marginal effect
#' @param focal_contrast Vector of length 2. Values of focal_var to contrast
#'   (e.g., c(0, 1) or c("pre", "post")). Effect = focal_contrast[2] - focal_contrast[1]
#' @param moderator Character. Optional name of moderating variable. Default NULL.
#' @param prob_col Character. Name of probability column. Default "probability"
#' @param category_col Character. Name of category column for ordinal models.
#'   Default "category". Set to NULL for binary/continuous outcomes.
#' @param conf_level Numeric. Confidence level. Default 0.95.
#'
#' @return Data frame with marginal effects and confidence intervals
#'
#' @examples
#' \dontrun{
#' # Binary outcome - simple marginal effect
#' me1 <- calculate_marginal_effect(
#'   predictions_long,
#'   focal_var = "prepost",
#'   focal_contrast = c(0, 1)
#' )
#'
#' # Binary outcome - moderated effect
#' me2 <- calculate_marginal_effect(
#'   predictions_long,
#'   focal_var = "prepost",
#'   focal_contrast = c(0, 1),
#'   moderator = "vote_trump"
#' )
#'
#' # Ordinal outcome with categories
#' me3 <- calculate_marginal_effect(
#'   predictions_long,
#'   focal_var = "prepost",
#'   focal_contrast = c(0, 1),
#'   moderator = "vote_trump",
#'   category_col = "category"
#' )
#' }
#'
#' @export
calculate_marginal_effect <- function(predictions_long,
                                      focal_var,
                                      focal_contrast,
                                      moderator = NULL,
                                      prob_col = "probability",
                                      category_col = "category",
                                      conf_level = 0.95) {

  require(dplyr)
  require(tidyr)

  # Input validation
  if (!focal_var %in% names(predictions_long)) {
    stop("focal_var '", focal_var, "' not found in predictions_long")
  }

  if (!prob_col %in% names(predictions_long)) {
    stop("prob_col '", prob_col, "' not found in predictions_long")
  }

  if (!"draw" %in% names(predictions_long)) {
    stop("predictions_long must contain a 'draw' column")
  }

  if (length(focal_contrast) != 2) {
    stop("focal_contrast must be a vector of length 2")
  }

  if (!is.null(moderator) && !moderator %in% names(predictions_long)) {
    stop("moderator '", moderator, "' not found in predictions_long")
  }

  # Check if ordinal (has category column)
  is_ordinal <- !is.null(category_col) && category_col %in% names(predictions_long)

  # Filter to only the two levels being contrasted
  contrast_data <- predictions_long %>%
    filter(.data[[focal_var]] %in% focal_contrast)

  # Check that both levels exist
  n_levels <- contrast_data %>%
    distinct(.data[[focal_var]]) %>%
    nrow()

  if (n_levels != 2) {
    stop("focal_contrast values not found in data. Available values: ",
         paste(unique(predictions_long[[focal_var]]), collapse = ", "))
  }

  # Separate the two conditions
  condition1 <- contrast_data %>%
    filter(.data[[focal_var]] == focal_contrast[1])

  condition2 <- contrast_data %>%
    filter(.data[[focal_var]] == focal_contrast[2])

  # Determine grouping variables for the join
  if (is_ordinal) {
    if (!is.null(moderator)) {
      join_vars <- c("draw", moderator, category_col)
      group_vars <- c(moderator, category_col)
    } else {
      join_vars <- c("draw", category_col)
      group_vars <- category_col
    }
  } else {
    if (!is.null(moderator)) {
      join_vars <- c("draw", moderator)
      group_vars <- moderator
    } else {
      join_vars <- "draw"
      group_vars <- NULL
    }
  }

  # Calculate marginal effect for each draw
  me_data <- condition2 %>%
    dplyr::select(all_of(c(join_vars, prob_col))) %>%
    rename(prob2 = !!prob_col) %>%
    left_join(
      condition1 %>%
        dplyr::select(all_of(c(join_vars, prob_col))) %>%
        rename(prob1 = !!prob_col),
      by = join_vars
    ) %>%
    mutate(marginal_effect = prob2 - prob1)

  # Calculate summary statistics
  lower_prob <- (1 - conf_level) / 2
  upper_prob <- 1 - lower_prob

  if (!is.null(group_vars)) {
    summary_data <- me_data %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        mean_effect = mean(marginal_effect, na.rm = TRUE),
        median_effect = median(marginal_effect, na.rm = TRUE),
        sd_effect = sd(marginal_effect, na.rm = TRUE),
        lower_ci = quantile(marginal_effect, lower_prob, na.rm = TRUE),
        upper_ci = quantile(marginal_effect, upper_prob, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    summary_data <- me_data %>%
      summarise(
        mean_effect = mean(marginal_effect, na.rm = TRUE),
        median_effect = median(marginal_effect, na.rm = TRUE),
        sd_effect = sd(marginal_effect, na.rm = TRUE),
        lower_ci = quantile(marginal_effect, lower_prob, na.rm = TRUE),
        upper_ci = quantile(marginal_effect, upper_prob, na.rm = TRUE)
      )
  }

  # Add metadata as attributes
  attr(summary_data, "focal_var") <- focal_var
  attr(summary_data, "focal_contrast") <- focal_contrast
  attr(summary_data, "moderator") <- moderator

  return(summary_data)
}
