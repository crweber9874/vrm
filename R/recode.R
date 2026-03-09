#' @title recodeColumn
#' @description Uniform recode function for survey data
#'
#' #'
#' @param data data frame
#' @param id specify a recode column
#' @param recode_rules specify the recode rules,  c("1" = 4, "2" = 3, "3" = 2, "4" = 1)
#' @param colname specify the new column name
#' @importFrom dplyr %>% select mutate row_number group_by ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom readr parse_number
#'
#' @return The data frame with recoded column
#' #'

#'
#' @export
#'
#'


recodeColumn <- function(data = df,
                         column,  # identify column
                         recode_rules, # recode rules
                         colname) {
  data %>%
    mutate(!!sym(colname) := case_when(
      as.numeric(!!sym(column)) %in% names(recode_rules) ~ recode_rules[as.character(as.numeric(!!sym(column)))],
      TRUE ~ NA
    ))
}
