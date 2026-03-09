
estimate_brms_models <- function(
    dependent_variable = "attend_march",
    independent_variable = "party_identification3",
    data = df,
    cores = 9,
    iterations = 1000,
    chains = 4,
    priors = c(prior(normal(0, 10), class = "b")),
    model_type = c("ordinal", "linear", "nominal", "binary"),
    ...
) {
  data[[independent_variable]] <- as.character(data[[independent_variable]])
  formula <- as.formula(paste(dependent_variable, "~", independent_variable))

  if (model_type == "ordinal") {
    data[[dependent_variable]] <- factor(data[[dependent_variable]], ordered = TRUE)
    model <- brms::brm(
      formula,
      data = data,
      family = cumulative("logit"),
      iter = iterations,
      chains = chains,
      cores = getOption("mc.cores", cores),
      prior = priors,
      ...
    )
  } else if (model_type == "nominal") {
    data[[dependent_variable]] <- as.character(data[[dependent_variable]])
    model <- brms::brm(
      formula,
      data = data,
      family = categorical("logit"),
      iter = iterations,
      chains = chains,
      cores = getOption("mc.cores", cores),
      ...
    )
  } else if (model_type == "binary") {
    if (length(unique(data[[dependent_variable]])) > 2) {
      stop("Binary models require a dependent variable with two levels.")
    } else {
      data[[dependent_variable]] <- as.numeric(data[[dependent_variable]])
      model <- brms::brm(
        formula,
        data = data,
        family = bernoulli("logit"),
        iter = iterations,
        chains = chains,
        cores = getOption("mc.cores", cores),
        prior = priors,
        ...
      )
    }
  } else if (model_type == "linear") {
    data[[dependent_variable]] <- as.numeric(data[[dependent_variable]])
    model <- brms::brm(
      formula,
      data = data,
      family = gaussian(),
      iter = iterations,
      chains = chains,
      cores = getOption("mc.cores", cores),
      prior = priors,
      ...
    )
  } else {
    stop("Invalid model type specified.")
  }

  if (model_type %in% c("ordinal", "nominal")) {
    dat <- tidyr::expand_grid(!!sym(independent_variable) := unique(na.omit(data[[independent_variable]]))) %>%
      as.data.frame() %>%
      tidybayes::add_epred_draws(model) %>%
      group_by(!!sym(independent_variable), .category) %>%
      summarize(
        .value = mean(.epred),
        .lower = quantile(.epred, 0.025),
        .upper = quantile(.epred, 0.975)
      ) %>%
      mutate(item = dependent_variable)
  } else {
    dat <- tidyr::expand_grid(!!sym(independent_variable) := unique(na.omit(data[[independent_variable]]))) %>%
      as.data.frame() %>%
      tidybayes::add_epred_draws(model) %>%
      group_by(!!sym(independent_variable)) %>%
      summarize(
        .value = mean(.epred),
        .lower = quantile(.epred, 0.025),
        .upper = quantile(.epred, 0.975)
      ) %>%
      mutate(item = dependent_variable)
  }

  return(list(model = model, dat = dat))
}
