#' Generate Predicted Category Probabilities from Ordered Logistic Regression
#'
#' @param design_matrix A data frame containing predictor variables.
#'   Can contain factors/characters - they will be automatically converted to dummy variables.
#' @param model A fitted ordered logistic regression model from MASS::polr().
#' @param n_draws Integer. Number of draws from the parameter distribution.
#' @param category_labels Character vector of labels for outcome categories.
#'
#' @return A data frame in long format with predicted probabilities
#'
#' @export
predict_ordinal_probs <- function(design_matrix,
                                  model,
                                  n_draws = 1000,
                                  category_labels = NULL) {

  require(MASS)
  require(dplyr)
  require(tidyr)

  # Validate inputs
  if (!inherits(model, "polr")) {
    stop("model must be a polr object from MASS::polr()")
  }

  if (!is.data.frame(design_matrix)) {
    design_matrix <- as.data.frame(design_matrix)
  }

  # Store original design matrix for output
  original_design <- design_matrix

  # Get model coefficients
  coef_names <- names(coefficients(model))

  if (length(coef_names) == 0) {
    stop("Model has no coefficients. Did you fit an intercept-only model?")
  }

  # Create model matrix to handle factors/dummy variables
  # Extract the formula from the model
  model_terms <- delete.response(terms(model))

  # Create model matrix from design_matrix
  # This will automatically create dummy variables
  X <- model.matrix(model_terms, data = design_matrix)

  # Remove intercept column if present (polr doesn't use it in the linear predictor)
  if ("(Intercept)" %in% colnames(X)) {
    X <- X[, -1, drop = FALSE]
  }

  # Check if we have all needed coefficients
  missing_vars <- setdiff(coef_names, colnames(X))
  if (length(missing_vars) > 0) {
    stop(
      "Model matrix is missing some predictor variables.\n",
      "Model coefficients: ", paste(coef_names, collapse = ", "), "\n",
      "Missing: ", paste(missing_vars, collapse = ", "), "\n",
      "Available in model matrix: ", paste(colnames(X), collapse = ", ")
    )
  }

  # Reorder columns to match coefficient order
  X <- X[, coef_names, drop = FALSE]

  # Get number of categories
  n_cutpoints <- length(model$zeta)
  n_categories <- n_cutpoints + 1

  # Set category labels
  if (is.null(category_labels)) {
    if (!is.null(model$lev)) {
      category_labels <- model$lev
    } else {
      category_labels <- paste0("Category_", 1:n_categories)
    }
  }

  if (length(category_labels) != n_categories) {
    stop("category_labels must have length ", n_categories)
  }

  # Draw parameters (coefficients + cutpoints)
  parameters <- mvrnorm(
    n_draws,
    mu = c(coefficients(model), model$zeta),
    Sigma = vcov(model)
  ) %>% as.data.frame()

  # Initialize list to store results for each row of design matrix
  results_list <- vector("list", nrow(X))

  # Loop through each row of design matrix
  for (i in 1:nrow(X)) {

    # Get coefficients
    coef_params <- as.matrix(parameters[, coef_names, drop = FALSE])

    # Get design values for this row
    design_row <- matrix(X[i, ], nrow = 1)

    # Calculate linear predictor: X'β
    linear_pred <- coef_params %*% t(design_row)
    linear_pred <- as.vector(linear_pred)

    # Calculate cumulative probabilities
    cum_probs <- matrix(NA, nrow = n_draws, ncol = n_cutpoints)

    for (j in 1:n_cutpoints) {
      cutpoint_name <- names(model$zeta)[j]
      cum_probs[, j] <- plogis(parameters[[cutpoint_name]] - linear_pred)
    }

    # Calculate category probabilities
    category_probs <- matrix(NA, nrow = n_draws, ncol = n_categories)

    # First category
    category_probs[, 1] <- cum_probs[, 1]

    # Middle categories
    if (n_categories > 2) {
      for (j in 2:(n_categories - 1)) {
        category_probs[, j] <- cum_probs[, j] - cum_probs[, j - 1]
      }
    }

    # Last category
    category_probs[, n_categories] <- 1 - cum_probs[, n_cutpoints]

    # Create data frame
    result_df <- data.frame(
      draw = 1:n_draws,
      category_probs
    )
    names(result_df)[2:(n_categories + 1)] <- category_labels

    # Add ORIGINAL design matrix values (not dummy variables)
    for (col in names(original_design)) {
      result_df[[col]] <- original_design[i, col]
    }

    results_list[[i]] <- result_df
  }

  # Combine and pivot to long format
  result_wide <- bind_rows(results_list)

  result_long <- result_wide %>%
    pivot_longer(
      cols = all_of(category_labels),
      names_to = "category",
      values_to = "probability"
    )

  return(result_long)
}
