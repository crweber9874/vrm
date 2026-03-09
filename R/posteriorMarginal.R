#' #' @title posterior_pme
#' Generate Predictive Marginal Effect
#'
#' Use category_groups to group categories together. The function asks the user to choose how to group
#' the categories together, in order to aid interpretation with many category models (e.g. ordinal or multinomial models).
#' The function returns the average marginal effect for each category, across levels of xvar, all other variables observed.
#' @param model {brms model}
#' @param xvar {independent variable}
#' @param xrange {values of xvar}
#' @param mvar {moderator variable}
#' @param mrange {values of mvar}
#' @return R data frame with posterior predictions for each category, across levels of xvar, all other variables held at mean.
#' @export

posterior_pme = function(model = burn_flag,
                         xvar = "prepost",
                         mvar = "presvote_trump_2020",
                         mrange =  c(0, 1),
                         xrange =  c("pre", "post")){
  data =  model$data
  cols_to_average <- setdiff(names(data)[-1], c(xvar, mvar))

  data_grid <- data %>%
    select(all_of(cols_to_average)) %>%
    summarize(across(everything(), mean)) %>%
    expand_grid(
      !!xvar := xrange,  # Use correct syntax to find min/max
      !!mvar := mrange) %>% add_epred_draws(model)
  t1 =
    data_grid %>%
    filter(!!sym(xvar) == "post") %>%
    subset(select = ".epred")

  t2 = data_grid %>%
    filter(!!sym(xvar) == "pre") %>%
    subset(select = ".epred")

  dat = data_grid %>%
    filter(!!sym(xvar) == "post")

  dat$me = t1$.epred - t2$.epred

  if (model$family[[1]] == "categorical" |
      model$family[[1]] == "cumulative"){
    plot = dat %>%
    subset(select = c("presvote_trump_2020", ".category", "me"))  %>%
    group_by(presvote_trump_2020, .category)
  }
  if (model$family[[1]] == "bernoulli" |
      model$family[[1]] == "gaussian"){
    plot = dat %>%
      subset(select = c("presvote_trump_2020",  "me"))  %>%
      group_by(presvote_trump_2020)
  }

  plot = plot %>%
      summarize(
      mean = mean(me),
      lower = quantile(me, 0.025),
      upper = quantile(me, 0.975)
    )
  return(plot)
}
