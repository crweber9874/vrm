#' Summarize Treatment Effects
#'
#' Aggregates individual-level treatment effects to compute various treatment effect estimates.
#' Computes mean, SD, and credible intervals across draws and individuals.
#'
#' @param effects_data Data frame from calculate_individual_effects() containing
#'   .draw, respondent_id, and a treatment effect column
#' @param effect_var Character. Name of treatment effect column. Default: "treatment_effect"
#' @param group_vars Character vector. Variables to group by when computing summaries.
#'   Default: NULL (compute overall ATE)
#' @param conf_level Numeric. Confidence level for credible intervals. Default: 0.95
#' @param draw_level Logical. If TRUE, first aggregates effects within each draw before
#'   computing across draws (preserves uncertainty). If FALSE, computes quantiles across
#'   all individual effects per group. Default: TRUE
#'
#' @return Data frame with treatment effect summaries:
#'   - mean_effect: mean treatment effect
#'   - median_effect: median treatment effect
#'   - sd_effect: standard deviation
#'   - lower_ci, upper_ci: credible interval bounds
#'   - Any specified group_vars
#'
#' @details
#' When draw_level = TRUE (recommended), the function:
#'   1. Aggregates to mean effect per draw per group
#'   2. Computes summaries across draws
#'
#' This approach preserves posterior uncertainty properly.
#'
#' @examples
#' \dontrun{
#' # Overall ATE
#' ate <- summarize_treatment_effects(
#'   ind_effects,
#'   effect_var = "treatment_effect"
#' )
#'
#' # CATE by vote choice and category
#' cate <- summarize_treatment_effects(
#'   ind_effects,
#'   effect_var = "treatment_effect",
#'   group_vars = c("vote_trump", ".category")
#' )
#'
#' # Display results
#' cate %>%
#'   gt() %>%
#'   fmt_number(columns = starts_with(c("mean", "sd", "lower", "upper")), decimals = 3)
#' }
#'
#' @export
summarize_treatment_effects <- function(effects_data,
                                        effect_var = "treatment_effect",
                                        group_vars = NULL,
                                        conf_level = 0.95,
                                        draw_level = TRUE) {

  require(dplyr)

  # Input validation
  if (!effect_var %in% names(effects_data)) {
    stop("effect_var '", effect_var, "' not found in effects_data")
  }

  if (!".draw" %in% names(effects_data)) {
    stop("effects_data must contain '.draw' column")
  }

  # Validate group_vars
  if (!is.null(group_vars)) {
    missing_vars <- setdiff(group_vars, names(effects_data))
    if (length(missing_vars) > 0) {
      stop("group_vars not found: ", paste(missing_vars, collapse = ", "))
    }
  }

  lower_prob <- (1 - conf_level) / 2
  upper_prob <- 1 - lower_prob

  if (draw_level) {
    # Aggregate to draw-level first
    if (is.null(group_vars)) {
      draw_agg <- effects_data %>%
        group_by(.draw) %>%
        summarise(
          effect_mean = mean(.data[[effect_var]], na.rm = TRUE),
          .groups = "drop"
        )

      summary <- draw_agg %>%
        summarise(
          mean_effect = mean(effect_mean, na.rm = TRUE),
          median_effect = median(effect_mean, na.rm = TRUE),
          sd_effect = sd(effect_mean, na.rm = TRUE),
          lower_ci = quantile(effect_mean, lower_prob, na.rm = TRUE),
          upper_ci = quantile(effect_mean, upper_prob, na.rm = TRUE)
        )
    } else {
      draw_agg <- effects_data %>%
        group_by(.draw, across(all_of(group_vars))) %>%
        summarise(
          effect_mean = mean(.data[[effect_var]], na.rm = TRUE),
          .groups = "drop"
        )

      summary <- draw_agg %>%
        group_by(across(all_of(group_vars))) %>%
        summarise(
          mean_effect = mean(effect_mean, na.rm = TRUE),
          median_effect = median(effect_mean, na.rm = TRUE),
          sd_effect = sd(effect_mean, na.rm = TRUE),
          lower_ci = quantile(effect_mean, lower_prob, na.rm = TRUE),
          upper_ci = quantile(effect_mean, upper_prob, na.rm = TRUE),
          .groups = "drop"
        )
    }
  } else {
    # Compute across all observations per group
    if (is.null(group_vars)) {
      summary <- effects_data %>%
        summarise(
          mean_effect = mean(.data[[effect_var]], na.rm = TRUE),
          median_effect = median(.data[[effect_var]], na.rm = TRUE),
          sd_effect = sd(.data[[effect_var]], na.rm = TRUE),
          lower_ci = quantile(.data[[effect_var]], lower_prob, na.rm = TRUE),
          upper_ci = quantile(.data[[effect_var]], upper_prob, na.rm = TRUE)
        )
    } else {
      summary <- effects_data %>%
        group_by(across(all_of(group_vars))) %>%
        summarise(
          mean_effect = mean(.data[[effect_var]], na.rm = TRUE),
          median_effect = median(.data[[effect_var]], na.rm = TRUE),
          sd_effect = sd(.data[[effect_var]], na.rm = TRUE),
          lower_ci = quantile(.data[[effect_var]], lower_prob, na.rm = TRUE),
          upper_ci = quantile(.data[[effect_var]], upper_prob, na.rm = TRUE),
          .groups = "drop"
        )
    }
  }

  return(summary)
}
