#' Generate Counterfactual Data for G-Computation
#'
#' Creates observed and counterfactual versions of data by inverting treatment assignment for binary treatment.
#' Used as input for g-computation style causal inference with BRMS models.
#'
#' @param data A data frame containing the data to be analyzed
#' @param treatment_var Character. Name of the treatment/exposure variable to flip
#'   (e.g., "prepost", "treatment_status")
#' @param flip_values Numeric vector of length 2. Values that will be swapped in counterfactual.
#'   Default: c(0, 1). The counterfactual flips from flip_values[1] to flip_values[2] and vice versa.
#' @param add_id_var Logical. Whether to add a respondent_id column for tracking individuals.
#'   Default: TRUE
#' @param keep_original Logical. Whether to keep original treatment value in column named
#'   'original_[treatment_var]'. Default: TRUE
#'
#' @return A data frame with rows doubled: original data tagged with pre_condition = "actual_treatment"
#'   and counterfactual data tagged with pre_condition = "hypothetical_treatment"
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' full_set <- generate_counterfactual_data(
#'   recode_data,
#'   treatment_var = "prepost"
#' )
#'
#' # Custom flip values
#' full_set <- generate_counterfactual_data(
#'   recode_data,
#'   treatment_var = "prepost",
#'   flip_values = c(0, 1)
#' )
#' }
#'
#' @export
generate_counterfactual_data <- function(data,
                                         treatment_var,
                                         flip_values = c(0, 1),
                                         add_id_var = TRUE,
                                         keep_original = TRUE) {

  require(dplyr)

  # Input validation
  if (!treatment_var %in% names(data)) {
    stop("treatment_var '", treatment_var, "' not found in data")
  }

  if (length(flip_values) != 2) {
    stop("flip_values must be a vector of length 2")
  }

  # Create observed data
  observed_data <- data %>%
    mutate(
      pre_condition = "actual_treatment"
    )

  # Add respondent ID if requested
  if (add_id_var) {
    observed_data <- observed_data %>%
      mutate(respondent_id = row_number())
  }

  # Keep original treatment value if requested
  if (keep_original) {
    original_col_name <- paste0("original_", treatment_var)
    observed_data <- observed_data %>%
      mutate(!!original_col_name := .data[[treatment_var]])
  }

  # Create counterfactual data by flipping treatment
  counterfactual_data <- data %>%
    mutate(
      pre_condition = "hypothetical_treatment",
      !!treatment_var := if_else(
        .data[[treatment_var]] == flip_values[1],
        flip_values[2],
        flip_values[1]
      )
    )

  if (add_id_var) {
    counterfactual_data <- counterfactual_data %>%
      mutate(respondent_id = row_number())
  }

  if (keep_original) {
    counterfactual_data <- counterfactual_data %>%
      mutate(!!original_col_name := .data[[treatment_var]])
    # Restore the flipped values
    counterfactual_data[[original_col_name]] <- data[[treatment_var]]
  }

  # Combine and return
  full_set <- bind_rows(observed_data, counterfactual_data)
  return(full_set)
}

