#' Prepare Prediction Data with Observed Counts
#'
#' Creates a data frame joining prediction summaries with observed counts,
#' allowing for dynamic grouping by different categorical variables.
#'
#' @param data A data frame containing the raw survey data
#' @param predictions Prediction object from a model to be summarized
#' @param category_var Name of the categorical variable (as string) to use for grouping.
#'   Default: "attend_march"
#' @param group_vars Character vector of additional grouping variables.
#'   Default: c("prepost", "vote_trump")
#'
#' @return A data frame with predictions joined to observed counts
#'
#' @examples
#' \dontrun{
#' # Using attend_march as category
#' plot_dat <- prepare_prediction_data(westernData, predictions_march)
#'
#' # Using a different category variable
#' plot_dat <- prepare_prediction_data(westernData, predictions_protest,
#'                                     category_var = "attend_protest")
#'
#' # Different grouping variables
#' plot_dat <- prepare_prediction_data(westernData, predictions,
#'                                     category_var = "support_policy",
#'                                     group_vars = c("prepost", "party_id"))
#' }
#'
#' @export
prepare_prediction_data <- function(data,
                                    predictions,
                                    category_var = "attend_march",
                                    group_vars = c("prepost", "vote_trump")) {

  # Create observed counts with dynamic grouping
  observed_counts <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, category_var)))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::rename(category = !!category_var) %>%
    dplyr::mutate(category = factor(category)) %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(group_vars), ~ !is.na(.)))

  # Join predictions with observed counts
  plot_dat <- summarize_predictions(predictions) %>%
    dplyr::mutate(category = factor(category)) %>%
    dplyr::left_join(observed_counts, by = c(group_vars, "category"))

  return(list(plotting_data = plot_dat, observed_counts = observed_counts))
}
