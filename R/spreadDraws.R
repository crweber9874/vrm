#' Merge Draws from BRMS object to Secondary Data
#'
#' @export
#'
spreadDraw <- function(model1, mean_name = "mean_var", lower_name = "lower.DIF", upper_name = "upper.DIF") {
  result <- spread_draws(model1, r_caseid__eta[caseid, ]) %>%
    group_by(caseid) %>%
    summarize(
      !!mean_name :=  mean(r_caseid__eta),
      !!lower_name := quantile(r_caseid__eta, 0.025),
      !!upper_name := quantile(r_caseid__eta, 0.975)
    )
  return(result)
}
