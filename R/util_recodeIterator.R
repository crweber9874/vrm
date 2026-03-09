#' Apply multiple recoding operations to a dataframe
#'
#' @param data A dataframe.
#' @param recode_list A list of recoding operations. Each operation is a list containing the column, recode_rules, and new_column.
#' @return A dataframe with the recoded columns.
#' @export
recodeList <- function(data, recode_list) {
  for (operation in recode_list) {
    data <- recodeColumn(data, operation$column, operation$recode_rules, operation$new_column)
  }
  return(data)
}
