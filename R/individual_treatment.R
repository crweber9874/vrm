#' Calculate Individual-Level Treatment Effects
#'
#' Converts long-format BRMS predictions (from add_epred_draws) into individual-level
#' treatment effects by comparing observed vs. counterfactual predictions.
#'
#' @param predictions_long Data frame in long format from tidybayes::add_epred_draws()
#'   Must contain: .draw, .epred (or similar prediction column), pre_condition,
#'   respondent_id, and original_[treatment_var]
#' @param pred_col Character. Name of prediction column. Default: ".epred"
#' @param treatment_var Character. Name of treatment variable. Used to locate
#'   'original_[treatment_var]' column for direction correction. Default: "prepost"
#' @param group_vars Character vector. Names of grouping variables to retain
#'   (e.g., c("vote_trump", ".category")). Default: NULL (no grouping)
#' @param effect_name Character. Name for the output effect column. Default: "treatment_effect"
#'
#' @return Data frame with individual-level treatment effects. Contains:
#'   - .draw: draw identifier
#'   - respondent_id: individual identifier
#'   - treatment_effect (or custom effect_name): difference between hypothetical and actual
#'   - Any specified group_vars
#'
#' @details
#' The function pivots predictions wide by pre_condition, calculates the difference,
#' and applies direction correction: if the individual was actually in the treatment
#' condition (original value = 1), the effect is negated to reflect the counterfactual
#' direction.
#'
#' @examples
#' \dontrun{
#' # Get individual effects with grouping
#' ind_effects <- calculate_individual_effects(
#'   preds_draws,
#'   pred_col = ".epred",
#'   treatment_var = "prepost",
#'   group_vars = c("vote_trump", ".category")
#' )
#'
#' # Without grouping (overall effects per individual)
#' ind_effects <- calculate_individual_effects(
#'   preds_draws,
#'   treatment_var = "prepost"
#' )
#' }
#'
#' @export
calculate_individual_effects <- function(predictions_long,
                                         pred_col = ".epred",
                                         treatment_var = "prepost",
                                         group_vars = NULL,
                                         effect_name = "treatment_effect") {

  require(dplyr)
  require(tidyr)

  # Input validation
  if (!pred_col %in% names(predictions_long)) {
    stop("pred_col '", pred_col, "' not found in predictions_long")
  }

  if (!"pre_condition" %in% names(predictions_long)) {
    stop("predictions_long must contain 'pre_condition' column")
  }

  if (!".draw" %in% names(predictions_long)) {
    stop("predictions_long must contain '.draw' column")
  }

  if (!"respondent_id" %in% names(predictions_long)) {
    stop("predictions_long must contain 'respondent_id' column")
  }

  original_col <- paste0("original_", treatment_var)
  if (!original_col %in% names(predictions_long)) {
    stop("predictions_long must contain '", original_col, "' column")
  }

  # Build pivot columns
  base_cols <- c(".draw", "respondent_id", original_col)
  if (!is.null(group_vars)) {
    # Verify group_vars exist
    missing_vars <- setdiff(group_vars, names(predictions_long))
    if (length(missing_vars) > 0) {
      stop("group_vars not found: ", paste(missing_vars, collapse = ", "))
    }
    base_cols <- c(base_cols, group_vars)
  }

  # Pivot wide by pre_condition
  effects_data <- predictions_long %>%
    dplyr::select(all_of(base_cols), pre_condition, !!pred_col) %>%
    pivot_wider(
      id_cols = all_of(base_cols),
      names_from = pre_condition,
      values_from = !!pred_col,
      names_prefix = "pred_"
    ) %>%
    # Calculate raw effect
    mutate(
      raw_effect = pred_hypothetical_treatment - pred_actual_treatment
    ) %>%
    # Apply direction correction: negate if individual was actually treated
    mutate(
      !!effect_name := if_else(
        .data[[original_col]] == 1,
        -raw_effect,
        raw_effect
      )
    ) %>%
    dplyr::select(-starts_with("pred_"), -raw_effect)

  return(effects_data)
}
