#' Reshape Wide Panel Data to Long (Person-Wave) Format
#'
#' Pivots wave-suffixed columns (e.g., `x1, x2` or `trust_2012, trust_2016`)
#' into one row per unit and wave. Columns not selected in `cols` are carried
#' along as time-invariant.
#'
#' @param data A wide data frame, one row per unit.
#' @param cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Time-varying
#'   columns whose names end in a wave number.
#' @param id Character. Unit identifier column. If `NULL` (default), a column
#'   named `"id"` is created from row numbers.
#' @param wave Character. Name of the wave column to create. Default `"wave"`.
#' @param pattern Regular expression with two capture groups: variable stem and
#'   wave number. The default handles `x1`, `x_1`, and `item_a_2012`.
#'
#' @return A tibble with `id`, `wave` (integer), one column per variable stem,
#'   and the carried-along columns.
#' @export
#'
#' @examples
#' wide <- data.frame(x1 = 1:3, x2 = 4:6, y_1 = 7:9, y_2 = 10:12, female = c(0, 1, 1))
#' panel_longer(wide, cols = c(x1:y_2))
panel_longer <- function(data, cols, id = NULL, wave = "wave",
                         pattern = "^(.*?)_?(\\d+)$") {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  sel <- names(tidyselect::eval_select(rlang::enquo(cols), data))
  if (length(sel) == 0) stop("`cols` selected no columns.", call. = FALSE)

  bad <- sel[!grepl(pattern, sel)]
  if (length(bad)) {
    stop("Column names do not match `pattern`: ", paste(bad, collapse = ", "), call. = FALSE)
  }

  if (is.null(id)) {
    if ("id" %in% names(data)) {
      stop("`data` already has an `id` column; pass `id = \"id\"` to use it.", call. = FALSE)
    }
    id <- "id"
    data <- dplyr::mutate(data, id = dplyr::row_number(), .before = 1)
  } else {
    .check_cols(data, id)
    if (anyDuplicated(data[[id]])) stop("`", id, "` is not unique in wide data.", call. = FALSE)
  }
  if (wave %in% names(data)) stop("`data` already has a `", wave, "` column.", call. = FALSE)

  tidyr::pivot_longer(
    data,
    cols = tidyselect::all_of(sel),
    names_to = c(".value", wave),
    names_pattern = pattern,
    names_transform = rlang::set_names(list(as.integer), wave)
  ) |>
    dplyr::arrange(.data[[id]], .data[[wave]])
}

#' Lag or Lead Variables Within Panel Units
#'
#' Adds lagged (or led) copies of variables, aligned by wave rather than by
#' row position. A unit missing wave *t - 1* receives `NA` at wave *t*,
#' instead of silently borrowing wave *t - 2* as
#' `dplyr::lag(x, order_by = wave)` would. Waves are indexed by their position
#' among the observed wave values, so unequally spaced waves (e.g., 2012, 2016,
#' 2018) are handled.
#'
#' @param data A long data frame, one row per unit-wave.
#' @param vars <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to lag.
#' @param id Character. Unit identifier column. Default `"id"`.
#' @param wave Character. Wave column (numeric or factor). Default `"wave"`.
#' @param n Integer. Number of waves to shift; negative values lead. Default `1`.
#' @param suffix Character suffix for new columns. Default `"_lag"`/`"_lead"`,
#'   with `abs(n)` appended when `abs(n) > 1`.
#'
#' @return `data` with added columns, in the original row order.
#' @export
#'
#' @examples
#' long <- data.frame(id = c(1, 1, 1, 2, 2), wave = c(1, 2, 3, 1, 3),
#'                    x = c(10, 11, 12, 20, 22))
#' panel_lag(long, x)
#' panel_lag(long, x, n = -1)
panel_lag <- function(data, vars, id = "id", wave = "wave", n = 1, suffix = NULL) {
  .check_panel(data, id, wave)
  sel <- names(tidyselect::eval_select(rlang::enquo(vars), data))
  if (length(sel) == 0) stop("`vars` selected no columns.", call. = FALSE)
  sel <- setdiff(sel, c(id, wave))
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n == 0 || n != round(n)) {
    stop("`n` must be a single non-zero integer.", call. = FALSE)
  }
  if (is.null(suffix)) {
    suffix <- paste0(if (n > 0) "_lag" else "_lead", if (abs(n) > 1) abs(n) else "")
  }
  new <- paste0(sel, suffix)
  clash <- intersect(new, names(data))
  if (length(clash)) stop("Columns already exist: ", paste(clash, collapse = ", "), call. = FALSE)

  w <- data[[wave]]
  levs <- if (is.factor(w)) levels(w) else sort(unique(w))
  idx <- match(as.character(w), as.character(levs))

  shifted <- data |>
    dplyr::select(tidyselect::all_of(c(id, sel))) |>
    dplyr::mutate(.vrm_idx = idx + n) |>
    dplyr::rename_with(~ paste0(.x, suffix), tidyselect::all_of(sel))

  data |>
    dplyr::mutate(.vrm_idx = idx) |>
    dplyr::left_join(shifted, by = c(id, ".vrm_idx")) |>
    dplyr::select(-".vrm_idx")
}

#' Within/Between Decomposition of Panel Variables
#'
#' Splits each variable into a unit mean (between) and a deviation from that
#' mean (within), the decomposition underlying fixed-effects, hybrid
#' (within-between), and RI-CLPM style models.
#'
#' @param data A long data frame, one row per unit-wave.
#' @param vars <[`tidy-select`][dplyr::dplyr_tidy_select]> Numeric columns
#'   to decompose.
#' @param id Character. Unit identifier column. Default `"id"`.
#' @param suffix Character vector of length 2: suffixes for the between and
#'   within components. Default `c("_between", "_within")`.
#' @param grand_center Logical. If `TRUE`, the between component is centered
#'   at the grand mean of unit means. Default `FALSE`.
#'
#' @return `data` with two added columns per variable.
#' @export
#'
#' @examples
#' long <- data.frame(id = c(1, 1, 2, 2), x = c(1, 3, 10, NA))
#' panel_center(long, x)
panel_center <- function(data, vars, id = "id", suffix = c("_between", "_within"),
                         grand_center = FALSE) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  .check_cols(data, id)
  if (!is.character(suffix) || length(suffix) != 2) {
    stop("`suffix` must be a character vector of length 2.", call. = FALSE)
  }
  sel <- setdiff(names(tidyselect::eval_select(rlang::enquo(vars), data)), id)
  if (length(sel) == 0) stop("`vars` selected no columns.", call. = FALSE)
  non_num <- sel[!purrr::map_lgl(data[sel], is.numeric)]
  if (length(non_num)) stop("Non-numeric columns: ", paste(non_num, collapse = ", "), call. = FALSE)
  new <- c(paste0(sel, suffix[1]), paste0(sel, suffix[2]))
  clash <- intersect(new, names(data))
  if (length(clash)) stop("Columns already exist: ", paste(clash, collapse = ", "), call. = FALSE)

  unit_mean <- function(x) {
    m <- mean(x, na.rm = TRUE)
    if (is.nan(m)) NA_real_ else m
  }

  out <- data |>
    dplyr::group_by(.data[[id]]) |>
    dplyr::mutate(dplyr::across(tidyselect::all_of(sel), unit_mean,
                                .names = paste0("{.col}", suffix[1]))) |>
    dplyr::ungroup()

  if (grand_center) {
    for (v in sel) {
      b <- paste0(v, suffix[1])
      gm <- mean(dplyr::distinct(out, .data[[id]], .data[[b]])[[b]], na.rm = TRUE)
      out[[b]] <- out[[b]] - gm
      out[[paste0(v, suffix[2])]] <- out[[v]] - (out[[b]] + gm)
    }
  } else {
    for (v in sel) out[[paste0(v, suffix[2])]] <- out[[v]] - out[[paste0(v, suffix[1])]]
  }
  out
}

.check_cols <- function(data, cols) {
  miss <- setdiff(cols, names(data))
  if (length(miss)) stop("Column(s) not found in `data`: ", paste(miss, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

.check_panel <- function(data, id, wave) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  .check_cols(data, c(id, wave))
  w <- data[[wave]]
  if (!(is.numeric(w) || is.factor(w))) stop("`", wave, "` must be numeric or factor.", call. = FALSE)
  if (anyNA(w) || anyNA(data[[id]])) stop("`", id, "` and `", wave, "` must not contain NA.", call. = FALSE)
  if (anyDuplicated(data[c(id, wave)])) {
    stop("Duplicate `", id, "`-`", wave, "` combinations found.", call. = FALSE)
  }
  invisible(TRUE)
}
