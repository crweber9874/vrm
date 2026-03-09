#' Summarize Predicted Probabilities
#'
#' Takes long-format prediction data with draws and creates summary statistics
#' including mean predictions and confidence intervals.
#'
#' @param predictions_long A data frame in long format containing:
#'   - One or more grouping variables (e.g., scenario characteristics)
#'   - A column with predicted probabilities from different draws
#' @param prob_col Character. Name of the column containing predicted probabilities.
#'   Default is "predicted_prob" for logistic regression or "probability" for ordinal.
#' @param group_vars Character vector of variable names to group by when calculating
#'   summary statistics. If NULL (default), will group by all columns except the
#'   probability column and any column named "draw".
#' @param conf_level Numeric. Confidence level for intervals. Default is 0.95.
#' @param quantiles Numeric vector. Additional quantiles to calculate beyond the
#'   confidence interval. Default is NULL.
#'
#' @return A data frame with summary statistics including:
#'   \item{mean_prob}{Mean predicted probability across draws}
#'   \item{median_prob}{Median predicted probability across draws}
#'   \item{sd_prob}{Standard deviation of predicted probability across draws}
#'   \item{lower_ci}{Lower confidence interval bound}
#'   \item{upper_ci}{Upper confidence interval bound}
#'   Additional columns from group_vars will be included.
#'
#' @examples
#' \dontrun{
#' # After generating predictions
#' predictions_long <- predict_logit_probs(design_matrix, model,
#'                                          n_draws = 1000,
#'                                          return_long = TRUE)
#' @export
summarize_predictions <- function(predictions_long,
                                  prob_col = NULL,
                                  group_vars = NULL,
                                  conf_level = 0.95,
                                  quantiles = NULL) {

  require(dplyr)

  # Input validation
  if (!is.data.frame(predictions_long)) {
    stop("predictions_long must be a data frame")
  }

  if (conf_level <= 0 || conf_level >= 1) {
    stop("conf_level must be between 0 and 1")
  }

  # Auto-detect probability column if not specified
  if (is.null(prob_col)) {
    if ("predicted_prob" %in% names(predictions_long)) {
      prob_col <- "predicted_prob"
    } else if ("probability" %in% names(predictions_long)) {
      prob_col <- "probability"
    } else {
      stop("Could not auto-detect probability column. Please specify prob_col parameter.\n",
           "Available columns: ", paste(names(predictions_long), collapse = ", "))
    }
  }

  # Check that prob_col exists
  if (!prob_col %in% names(predictions_long)) {
    stop("Column '", prob_col, "' not found in predictions_long.\n",
         "Available columns: ", paste(names(predictions_long), collapse = ", "))
  }

  # If group_vars is NULL, auto-detect: use all columns except prob_col and "draw"
  if (is.null(group_vars)) {
    exclude_cols <- c(prob_col, "draw")
    group_vars <- setdiff(names(predictions_long), exclude_cols)

    if (length(group_vars) == 0) {
      # No grouping variables - create overall summary
      group_vars <- NULL
    }
  }

  # Validate group_vars exist
  if (!is.null(group_vars)) {
    missing_vars <- setdiff(group_vars, names(predictions_long))
    if (length(missing_vars) > 0) {
      stop("group_vars not found in predictions_long: ",
           paste(missing_vars, collapse = ", "))
    }
  }

  # Calculate quantile probabilities
  lower_prob <- (1 - conf_level) / 2
  upper_prob <- 1 - lower_prob

  if (is.null(group_vars)) {
    summary_data <- predictions_long |>
      summarise(
        mean_prob = mean(.data[[prob_col]], na.rm = TRUE),
        median_prob = median(.data[[prob_col]], na.rm = TRUE),
        sd_prob = sd(.data[[prob_col]], na.rm = TRUE),
        lower_ci = quantile(.data[[prob_col]], lower_prob, na.rm = TRUE),
        upper_ci = quantile(.data[[prob_col]], upper_prob, na.rm = TRUE)
      )

    # Add additional quantiles if requested
    if (!is.null(quantiles)) {
      for (q in quantiles) {
        q_name <- paste0("q_", sprintf("%.3f", q))
        summary_data[[q_name]] <- quantile(predictions_long[[prob_col]], q, na.rm = TRUE)
      }
    }

  } else {
    # Group by specified variables
    summary_data <- predictions_long |>
      group_by(across(all_of(group_vars))) |>
      summarise(
        mean_prob = mean(.data[[prob_col]], na.rm = TRUE),
        median_prob = median(.data[[prob_col]], na.rm = TRUE),
        sd_prob = sd(.data[[prob_col]], na.rm = TRUE),
        lower_ci = quantile(.data[[prob_col]], lower_prob, na.rm = TRUE),
        upper_ci = quantile(.data[[prob_col]], upper_prob, na.rm = TRUE),
        .groups = "drop"
      )

    if (!is.null(quantiles)) {
      for (q in quantiles) {
        q_name <- paste0("q_", sprintf("%.3f", q))

        quant_data <- predictions_long |>
          group_by(across(all_of(group_vars))) |>
          summarise(
            !!q_name := quantile(.data[[prob_col]], q, na.rm = TRUE),
            .groups = "drop"
          )

        summary_data <- left_join(summary_data, quant_data, by = group_vars)
      }
    }
  }

  return(summary_data)
}
