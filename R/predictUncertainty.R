#' Generate Predicted Probabilities with Uncertainty from Logistic Regression
#'
#' @param design_matrix A data frame or matrix containing the predictor variables.
#' @param model A fitted model object (e.g., from glm()).
#' @param n_draws Integer. Number of draws from the parameter distribution. Default is 1000.
#' @param group_vars Character vector of variable names to group by. If NULL (default),
#'   groups by all variables in design_matrix to return one row per scenario.
#' @param conf_level Numeric. Confidence level for intervals. Default is 0.95.
#'
#' @return A data frame with summary statistics
#'
#' @export
predict_logit_probs <- function(design_matrix,
                                model,
                                n_draws = 1000,
                                group_vars = NULL,
                                conf_level = 0.95) {

  require(MASS)
  require(dplyr)
  require(tidyr)

  # Input validation
  if (!inherits(design_matrix, c("data.frame", "matrix"))) {
    stop("design_matrix must be a data frame or matrix")
  }

  tryCatch({
    coef(model)
    vcov(model)
  }, error = function(e) {
    stop("model must have coef() and vcov() methods that work")
  })

  if (n_draws < 1 || !is.numeric(n_draws)) {
    stop("n_draws must be a positive integer")
  }

  if (conf_level <= 0 || conf_level >= 1) {
    stop("conf_level must be between 0 and 1")
  }

  # Convert to data frame if matrix
  if (is.matrix(design_matrix)) {
    design_matrix <- as.data.frame(design_matrix)
  }

  # Store original design matrix for output
  original_design <- design_matrix

  # Get the original data used to fit the model
  model_data <- model$model

  # For each variable in design_matrix, ensure factor levels match the original model
  for (var_name in names(design_matrix)) {
    if (var_name %in% names(model_data)) {
      # If variable is character or factor in design_matrix
      if (is.character(design_matrix[[var_name]]) || is.factor(design_matrix[[var_name]])) {
        # Get original levels from model data
        if (is.factor(model_data[[var_name]])) {
          original_levels <- levels(model_data[[var_name]])
        } else if (is.character(model_data[[var_name]])) {
          original_levels <- unique(model_data[[var_name]])
        } else {
          next
        }

        # Convert to factor with ALL original levels (even if not in design_matrix)
        # This ensures model.matrix creates all necessary dummy columns
        design_matrix[[var_name]] <- factor(design_matrix[[var_name]],
                                            levels = original_levels)
      }
    }
  }

  # Create model matrix to handle factors/dummy variables
  model_terms <- delete.response(terms(model))

  # Use xlev argument to ensure all factor levels are recognized
  # Extract xlevels from the model
  xlev <- model$xlevels

  X <- model.matrix(model_terms, data = design_matrix, xlev = xlev)

  # Get coefficient names
  coef_names <- names(coef(model))

  # Check that model matrix has all needed variables
  missing_vars <- setdiff(coef_names, colnames(X))
  if (length(missing_vars) > 0) {
    stop(
      "Model matrix is missing some predictor variables.\n",
      "Model coefficients needed: ", paste(coef_names, collapse = ", "), "\n",
      "Missing from model matrix: ", paste(missing_vars, collapse = ", "), "\n",
      "Model matrix columns: ", paste(colnames(X), collapse = ", "), "\n",
      "Tip: Make sure all factor levels from the original model are present."
    )
  }

  # Reorder columns to match coefficient order
  X <- X[, coef_names, drop = FALSE]

  # Calculate quantile probabilities
  lower_prob <- (1 - conf_level) / 2
  upper_prob <- 1 - lower_prob

  # Draw parameters from multivariate normal distribution
  parameter_draws <- mvrnorm(
    n = n_draws,
    mu = coef(model),
    Sigma = vcov(model)
  )

  # Generate predictions for each draw
  sampled_predictions <- X %*% t(parameter_draws)
  sampled_predictions <- plogis(sampled_predictions)

  # Convert to data frame
  sampled_predictions <- as.data.frame(sampled_predictions)
  names(sampled_predictions) <- paste0("draw_", seq_len(n_draws))

  # Combine ORIGINAL design matrix with predictions
  dat_wide <- cbind(original_design, sampled_predictions)

  # Pivot to long format
  dat_long <- dat_wide %>%
    pivot_longer(
      cols = starts_with("draw_"),
      names_to = "draw",
      values_to = "predicted_prob"
    )

  # If group_vars is NULL, default to all columns in original design
  if (is.null(group_vars)) {
    group_vars <- names(original_design)
  }

  # Create summary statistics
  if (length(group_vars) == 0) {
    summary_data <- dat_long %>%
      summarise(
        mean_prob = mean(predicted_prob),
        lower_ci = quantile(predicted_prob, lower_prob),
        upper_ci = quantile(predicted_prob, upper_prob)
      )
  } else {
    # Check that group_vars exist in design_matrix
    missing_vars <- setdiff(group_vars, names(original_design))
    if (length(missing_vars) > 0) {
      stop("group_vars not found in design_matrix: ",
           paste(missing_vars, collapse = ", "))
    }
  }

  return(dat_long)
}
