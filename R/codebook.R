#' Build a codebook from recode rules
#'
#' Generates a codebook data frame (and optionally a `gt` table) from the same
#' `recode_rules` list structure used by [recodeList()].
#'
#' @param recode_list A named or unnamed list of recode operations, each a list
#'   with `column`, `recode_rules`, and `new_column`. Optionally include
#'   `section` and `description` fields for richer output.
#' @param data Optional data frame. If supplied, summary statistics (n, n_miss,
#'   mean, sd) are computed for each variable present in the data.
#' @param render Logical; if `TRUE` (default `FALSE`) and the `gt` package is
#'   available, return a formatted `gt` table instead of a data frame.
#' @param title Character string for the table title when `render = TRUE`.
#'
#' @return A `tibble` with columns `section`, `variable`, `source`, `metric`,
#'   `description`, and (if `data` is supplied) `n`, `n_miss`, `mean`, `sd`.
#'   If `render = TRUE`, a `gt` object.
#'
#' @details
#' The function infers the response metric from the recode rules:
#' \itemize{
#'   \item Named vectors matching known vrm vectors (`five_r`, `five_n`, etc.)
#'         are labelled accordingly.
#'   \item Two-value rules mapping to 0/1 are labelled "binary".
#'   \item Rules with character values are labelled "categorical".
#'   \item Otherwise the range of output values is reported.
#' }
#'
#' To add section headers and descriptions, include them in each rule list:
#' \preformatted{
#' list(column = "WSS10_1", recode_rules = five_r,
#'      new_column = "sdo_group_inferiority",
#'      section = "SDO", description = "Some groups are inferior")
#' }
#'
#' @examples
#' \dontrun{
#' rules <- list(
#'   list(column = "WSS10_1", recode_rules = five_r,
#'        new_column = "sdo_group_inferiority",
#'        section = "SDO", description = "Group inferiority"),
#'   list(column = "WSS10_2", recode_rules = five_n,
#'        new_column = "sdo_unequal_deserving",
#'        section = "SDO", description = "Unequal deserving")
#' )
#' codebook(rules)
#' codebook(rules, data = wss20, render = TRUE)
#' }
#'
#' @importFrom dplyr tibble
#' @export
codebook <- function(recode_list,
                     data = NULL,
                     render = FALSE,
                     title = "Variable Codebook") {

  rows <- lapply(recode_list, function(op) {
    metric <- infer_metric(op$recode_rules)
    tibble::tibble(
      section     = op$section     %||% "",
      variable    = op$new_column,
      source      = op$column,
      metric      = metric,
      description = op$description %||% ""
    )
  })
  cb <- do.call(rbind, rows)

  # Add summary stats if data is supplied

  if (!is.null(data)) {
    cb$n      <- NA_integer_
    cb$n_miss <- NA_integer_
    cb$mean   <- NA_real_
    cb$sd     <- NA_real_
    for (i in seq_len(nrow(cb))) {
      v <- cb$variable[i]
      if (v %in% names(data)) {
        vals <- data[[v]]
        num_vals <- suppressWarnings(as.numeric(vals))
        cb$n[i]      <- sum(!is.na(num_vals))
        cb$n_miss[i] <- sum(is.na(num_vals))
        cb$mean[i]   <- mean(num_vals, na.rm = TRUE)
        cb$sd[i]     <- stats::sd(num_vals, na.rm = TRUE)
      }
    }
  }

  if (render) {
    if (!requireNamespace("gt", quietly = TRUE)) {
      warning("gt package not available; returning data frame.")
      return(cb)
    }
    return(render_codebook(cb, title = title, has_stats = !is.null(data)))
  }

  cb
}

#' Infer metric label from a recode_rules vector
#' @param rules A named character/numeric vector of recode rules.
#' @return A character string describing the metric.
#' @keywords internal
infer_metric <- function(rules) {
  vals <- unname(rules)
  num_vals <- suppressWarnings(as.numeric(vals))

  # Check if all output values are character (categorical)
  if (all(is.na(num_vals)) && !all(is.na(vals))) {
    return("categorical")
  }

  # Binary detection
  if (length(rules) == 2 && all(sort(num_vals) == c(0, 1))) {
    return("binary")
  }

  # Detect known vrm vectors by signature
  n <- length(rules)
  keys <- sort(as.numeric(names(rules)))
  sorted_vals <- num_vals[order(as.numeric(names(rules)))]

  if (n == 5 && all(keys == 1:5)) {
    if (all(sorted_vals == 5:1)) return("5-pt rev")
    if (all(sorted_vals == 1:5)) return("5-pt")
  }
  if (n == 4 && all(keys == 1:4)) {
    if (all(sorted_vals == 4:1)) return("4-pt rev")
    if (all(sorted_vals == 1:4)) return("4-pt")
  }
  if (n == 3 && all(keys == 1:3)) {
    if (all(sorted_vals == 3:1)) return("3-pt rev")
    if (all(sorted_vals == 1:3)) return("3-pt")
  }
  if (n == 7 && all(keys == 1:7)) {
    if (all(sorted_vals == 1:7)) return("7-pt")
    if (all(sorted_vals == 7:1)) return("7-pt rev")
  }

  # Fallback: report range
  rng <- range(num_vals, na.rm = TRUE)
  if (all(is.finite(rng))) {
    return(paste0(rng[1], "-", rng[2]))
  }

  "unknown"
}

#' Render a codebook data frame as a gt table
#' @param cb A codebook tibble from [codebook()].
#' @param title Table title.
#' @param has_stats Logical; whether summary stats columns are present.
#' @return A `gt` object.
#' @keywords internal
render_codebook <- function(cb, title = "Variable Codebook", has_stats = FALSE) {
  tbl <- gt::gt(cb, groupname_col = "section")
  tbl <- gt::tab_header(tbl, title = title)
  tbl <- gt::cols_label(tbl,
    variable    = "Variable",
    source      = "Source",
    metric      = "Metric",
    description = "Description"
  )
  tbl <- gt::tab_style(tbl,
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups()
  )
  tbl <- gt::tab_style(tbl,
    style = gt::cell_text(font = "monospace", size = "small"),
    locations = gt::cells_body(columns = c("variable", "source"))
  )

  if (has_stats) {
    tbl <- gt::cols_label(tbl,
      n      = "N",
      n_miss = "Missing",
      mean   = "Mean",
      sd     = "SD"
    )
    tbl <- gt::fmt_number(tbl,
      columns = c("mean", "sd"),
      decimals = 2
    )
  }

  tbl
}
