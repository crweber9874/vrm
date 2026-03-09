#' Fit Ordered Logit Model
#'
#' This function fits an ordered logistic regression model using the proportional odds assumption.
#'
#' @param formula A formula specifying the model. The left-hand side should be the ordered factor response.
#' @param data A data frame containing the variables in the formula.
#' @param ... Additional arguments passed to MASS::polr()
#'
#' @return An object of class "polr" containing the fitted model.
#' @export
#' @importFrom MASS polr
#'
#' @examples
#' \dontrun{
#' # Create sample ordered data
#' set.seed(123)
#' n <- 100
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#' latent <- 0.5 * x1 + 0.3 * x2 + rnorm(n)
#' y <- cut(latent, breaks = c(-Inf, -1, 0, 1, Inf), labels = c("Poor", "Fair", "Good", "Excellent"))
#' df <- data.frame(y = y, x1 = x1, x2 = x2)
#' 
#' # Fit ordered logit model
#' model <- fit_ordered_logit(y ~ x1 + x2, data = df)
#' summary(model)
#' }
fit_ordered_logit <- function(formula, data, ...) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' is required. Please install it.")
  }
  
  model <- MASS::polr(formula = formula, data = data, method = "logistic", Hess = TRUE, ...)
  return(model)
}

#' Fit Ordered Probit Model
#'
#' This function fits an ordered probit regression model using the proportional odds assumption.
#'
#' @param formula A formula specifying the model. The left-hand side should be the ordered factor response.
#' @param data A data frame containing the variables in the formula.
#' @param ... Additional arguments passed to MASS::polr()
#'
#' @return An object of class "polr" containing the fitted model.
#' @export
#' @importFrom MASS polr
#'
#' @examples
#' \dontrun{
#' # Create sample ordered data
#' set.seed(123)
#' n <- 100
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#' latent <- 0.5 * x1 + 0.3 * x2 + rnorm(n)
#' y <- cut(latent, breaks = c(-Inf, -1, 0, 1, Inf), labels = c("Poor", "Fair", "Good", "Excellent"))
#' df <- data.frame(y = y, x1 = x1, x2 = x2)
#' 
#' # Fit ordered probit model
#' model <- fit_ordered_probit(y ~ x1 + x2, data = df)
#' summary(model)
#' }
fit_ordered_probit <- function(formula, data, ...) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' is required. Please install it.")
  }
  
  model <- MASS::polr(formula = formula, data = data, method = "probit", Hess = TRUE, ...)
  return(model)
}

#' Predict from Ordered Regression Model
#'
#' Generate predictions from a fitted ordered logit or probit model.
#'
#' @param model A fitted model object from fit_ordered_logit() or fit_ordered_probit().
#' @param newdata Optional data frame for which to generate predictions. If omitted, uses the original data from the model.
#' @param type The type of prediction: "class" for predicted categories or "probs" for probabilities.
#'
#' @return Predicted values or probabilities.
#' @export
#' @importFrom stats predict
#'
#' @examples
#' \dontrun{
#' # Using the model from previous example
#' # For new data
#' new_data <- data.frame(x1 = 0.5, x2 = 0.3)
#' predictions <- predict_ordered(model, newdata = new_data, type = "class")
#' probabilities <- predict_ordered(model, newdata = new_data, type = "probs")
#' }
predict_ordered <- function(model, newdata = NULL, type = c("class", "probs")) {
  type <- match.arg(type)
  
  if (type == "class") {
    if (is.null(newdata)) {
      # Use fitted values for predictions on original data
      return(model$fitted.values)
    } else {
      return(predict(model, newdata = newdata, type = "class"))
    }
  } else {
    return(predict(model, newdata = newdata, type = "probs"))
  }
}

#' Summary Statistics for Ordered Regression Model
#'
#' Provides enhanced summary statistics for ordered logit and probit models.
#'
#' @param model A fitted model object from fit_ordered_logit() or fit_ordered_probit().
#'
#' @return A list containing coefficients, standard errors, z-values, p-values, and model fit statistics.
#' @export
#' @importFrom stats coef vcov pnorm
#'
#' @examples
#' \dontrun{
#' # Using the model from previous example
#' model_summary <- summary_ordered(model)
#' print(model_summary)
#' }
summary_ordered <- function(model) {
  # Get the summary from MASS
  model_summary <- summary(model)
  
  # Extract the coefficient table
  coef_table <- coef(model_summary)
  
  # Get p-values (column name might vary)
  if ("Pr(>|t|)" %in% colnames(coef_table)) {
    p_val <- coef_table[, "Pr(>|t|)"]
  } else if ("Pr(>|z|)" %in% colnames(coef_table)) {
    p_val <- coef_table[, "Pr(>|z|)"]
  } else {
    # Calculate p-values manually from z-values
    z_vals <- coef_table[, "t value"]
    p_val <- 2 * (1 - pnorm(abs(z_vals)))
  }
  
  # Add significance stars
  signif <- ifelse(p_val < 0.001, "***",
                   ifelse(p_val < 0.01, "**",
                          ifelse(p_val < 0.05, "*",
                                 ifelse(p_val < 0.1, ".", ""))))
  
  coef_table <- cbind(coef_table, Signif = signif)
  
  result <- list(
    coefficients = coef_table,
    method = model$method,
    n_obs = length(model$fitted.values),
    loglik = model$deviance,
    model = model
  )
  
  class(result) <- "summary.ordered"
  return(result)
}
