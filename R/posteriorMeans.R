#' Generate Posterior Predictions with 2 way interactions
#'
#' Use category_groups to group categories together. The function asks the user to choose how to group
#' the categories together, in order to aid interpretation with many category models (e.g. ordinal or multinomial models).
#' @param model {brms model}
#' @param xvar {independent variable}
#' @param xrange {values of xvar}
#' @param mvar {moderator variable}
#' @param mrange {values of mvar}
#' @return R data frame with posterior predictions for each category, across levels of xvar, all other variables held at mean.
#' @export
#'
#
posterior_means <- function(
    model = burn_flag,
    xvar = "presvote_trump_2020",
    mvar = "prepost",
    xval =  seq(0, 1),
    mval =  c(0, 1)
) {
  formula     =    model$formula
  data =  model$data
  cols_to_average <- setdiff(names(data)[-1], c(xvar, mvar))
  data_grid <- data %>%
    select(all_of(cols_to_average)) %>%
    summarize(across(everything(), mean)) %>%
    expand_grid(
      !!xvar := xval,
      !!mvar := mval
    ) %>% add_epred_draws(model)
  if (model$family[[1]] == "categorical" |
      model$family[[1]] == "cumulative"){
    print("categorical or cumulative")
    plot = data_grid %>%
      group_by(!!sym(xvar), !!sym(mvar), .category) %>%
      summarize(
        mean = mean(.epred),
        lower = quantile(.epred, 0.025),
        upper = quantile(.epred, 0.975))  }
  if (model$family[[1]] == "bernoulli" |
      model$family[[1]] == "gaussian"){
    print("bernoulli or gaussian")
    plot = data_grid %>%
      group_by(!!sym(xvar), !!sym(mvar)) %>%
      summarize(
        mean = mean(.epred),
        lower = quantile(.epred, 0.025),
        upper = quantile(.epred, 0.975))
  }
  return(plot)
}
