#' @title irt_prepare
#' @description Validate an item set and reshape it into the person-by-item long
#'   format a \pkg{brms} item response model expects. This is the data-side half
#'   of [brms_irt()] and is exported so the reshaping can be inspected -- or
#'   unit-tested -- without fitting anything.
#'
#' @param data A data frame in wide form: one row per respondent, one column
#'   per item.
#' @param items Character vector of at least two column names in `data` that
#'   load on the latent variable.
#' @param type Measurement level of the items. One of `"ordinal"`,
#'   `"nominal"`, or `"binary"`. There is no default: the response family and
#'   the admissible number of categories both follow from it, so the caller
#'   must say which it is.
#' @param id Optional name of a column in `data` that uniquely identifies
#'   respondents. When `NULL` (the default) the row number is used and returned
#'   in a column called `.row_id`.
#'
#' @return A list with components
#'   \describe{
#'     \item{long}{A tibble with columns `.id`, `item`, `response`, plus the
#'       identifier column.}
#'     \item{id_col}{Name of the identifier column.}
#'     \item{items}{The validated item names, in the order supplied.}
#'     \item{type}{The validated measurement level.}
#'     \item{levels}{Response categories, in order, as used for the model.}
#'     \item{n_obs}{Number of non-missing person-item responses modelled.}
#'     \item{n_miss}{Number of person-item cells that are missing.}
#'     \item{n_persons}{Number of respondents contributing a response.}
#'   }
#'
#' @details Items are harmonised onto one response scale before reshaping,
#'   because a long-format model needs a single response column:
#'   * `binary` items must have exactly two observed values across the set,
#'     mapped to 0/1 in sorted order.
#'   * `ordinal` items are mapped onto the union of their sorted observed values
#'     and returned as an ordered factor, so category *k* means the same thing
#'     for every item.
#'   * `nominal` items are returned as an unordered factor over the union of
#'     their observed values.
#'
#'   Missing responses are dropped from the long frame -- they carry no
#'   information about the latent score -- but are counted and reported, so
#'   `n_obs` and `n_miss` always travel with the estimates.
#'
#' @seealso [irt_formula()], [brms_irt()]
#' @examples
#' d <- data.frame(a = c(1, 2, 3), b = c(3, 2, 1), c = c(2, 2, 3))
#' prep <- irt_prepare(d, items = c("a", "b", "c"), type = "ordinal")
#' prep$n_obs
#' @export
irt_prepare <- function(data, items, type, id = NULL) {
  if (!is.data.frame(data))
    stop("`data` must be a data frame.", call. = FALSE)
  if (missing(type))
    stop("`type` must be given explicitly: one of \"ordinal\", \"nominal\", ",
         "or \"binary\".", call. = FALSE)
  type <- match.arg(type, c("ordinal", "nominal", "binary"))

  if (!is.character(items) || length(items) < 2L)
    stop("`items` must be a character vector naming at least two indicators.",
         call. = FALSE)
  if (anyDuplicated(items))
    stop("`items` contains duplicates: ",
         paste(unique(items[duplicated(items)]), collapse = ", "), call. = FALSE)
  absent <- setdiff(items, names(data))
  if (length(absent))
    stop("item(s) not found in `data`: ", paste(absent, collapse = ", "),
         call. = FALSE)

  id_col <- irt_id_column(data, id)
  if (identical(id_col, ".row_id")) data[[id_col]] <- seq_len(nrow(data))

  resp <- lapply(data[items], irt_strip_labels)
  observed <- sort(unique(unlist(lapply(resp, function(x) unique(x[!is.na(x)])))))
  if (!length(observed))
    stop("every item is entirely missing.", call. = FALSE)

  per_item <- vapply(resp, function(x) length(unique(x[!is.na(x)])), integer(1))
  if (type == "binary") {
    bad <- names(per_item)[per_item > 2L]
    if (length(bad))
      stop("`type = \"binary\"` but these items have more than two observed ",
           "values: ", paste(bad, collapse = ", "), call. = FALSE)
    if (length(observed) != 2L)
      stop("`type = \"binary\"` needs exactly two observed values across the ",
           "item set; found ", length(observed), ".", call. = FALSE)
  } else {
    flat <- names(per_item)[per_item < 2L]
    if (length(flat))
      stop("these items have fewer than two observed values and cannot inform ",
           "a latent variable: ", paste(flat, collapse = ", "), call. = FALSE)
    if (type == "ordinal" && length(observed) < 3L)
      warning("only ", length(observed), " response categories observed; a ",
              "binary model may be more appropriate.", call. = FALSE)
  }

  n_cells <- length(items) * nrow(data)
  n_miss  <- sum(vapply(resp, function(x) sum(is.na(x)), integer(1)))

  wide <- data[c(id_col, items)]
  wide[items] <- resp
  long <- tidyr::pivot_longer(
    tibble::as_tibble(wide),
    cols = dplyr::all_of(items), names_to = "item", values_to = "response"
  )
  long <- dplyr::filter(long, !is.na(.data$response))
  long$item <- factor(long$item, levels = items)
  long$.id  <- factor(long[[id_col]], levels = unique(as.character(data[[id_col]])))
  long$response <- switch(
    type,
    binary  = as.integer(match(long$response, observed) - 1L),
    ordinal = factor(long$response, levels = observed, ordered = TRUE),
    nominal = factor(long$response, levels = observed)
  )
  long <- dplyr::relocate(long, ".id", "item", "response")

  stopifnot(nrow(long) == n_cells - n_miss)

  list(long = long, id_col = id_col, items = items, type = type,
       levels = observed, n_obs = nrow(long), n_miss = n_miss,
       n_persons = dplyr::n_distinct(long$.id))
}

#' Resolve (and validate) the respondent identifier column
#' @param data A data frame.
#' @param id A column name or `NULL`.
#' @return The identifier column name; `".row_id"` when `id` is `NULL`.
#' @keywords internal
#' @noRd
irt_id_column <- function(data, id = NULL) {
  if (is.null(id)) {
    if (".row_id" %in% names(data))
      stop("`data` already has a `.row_id` column; pass `id = \".row_id\"` to ",
           "use it, or rename it.", call. = FALSE)
    return(".row_id")
  }
  if (!is.character(id) || length(id) != 1L)
    stop("`id` must be a single column name or NULL.", call. = FALSE)
  if (!id %in% names(data))
    stop("`id` column not found in `data`: ", id, call. = FALSE)
  if (anyNA(data[[id]]))
    stop("`id` column has missing values; it must identify every row.",
         call. = FALSE)
  if (anyDuplicated(data[[id]]))
    stop("`id` column does not uniquely identify rows.", call. = FALSE)
  id
}

#' Drop haven/labelled attributes so items can be pooled onto one scale
#' @param x A vector.
#' @return An unlabelled atomic vector.
#' @keywords internal
#' @noRd
irt_strip_labels <- function(x) {
  if (is.factor(x)) return(as.character(x))
  attributes(x) <- NULL
  x
}

#' @title irt_formula
#' @description Build the \pkg{brms} formula, response family, and default
#'   priors for an item response model. Separated from [brms_irt()] so a
#'   specification can be printed or checked before anything is sampled.
#'
#' @param type Measurement level: `"ordinal"`, `"nominal"`, or `"binary"`.
#' @param model `"1pl"` (equal discriminations -- the Rasch / partial-credit
#'   family) or `"2pl"` (item-specific discriminations). `"2pl"` is not
#'   available for nominal items, whose likelihood already carries
#'   category-specific slopes.
#' @param thresholds Ordinal items only. `"common"` gives every item the same
#'   thresholds and lets a random item intercept shift difficulty; `"item"`
#'   estimates a separate threshold vector per item (a graded response model),
#'   in which case the item intercept is dropped to keep the model identified.
#'
#' @return A list with the `brmsformula`, the `family`, default `prior`
#'   (`NULL` for nominal items, where brms's own defaults are used), a one-line
#'   `label`, and the resolved `type`, `model`, and `thresholds`.
#'
#' @seealso [irt_prepare()], [brms_irt()]
#' @examples
#' \dontrun{
#' irt_formula("binary", model = "1pl")$label
#' }
#' @export
irt_formula <- function(type, model = c("1pl", "2pl"),
                        thresholds = c("common", "item")) {
  if (missing(type))
    stop("`type` must be given explicitly.", call. = FALSE)
  type       <- match.arg(type, c("ordinal", "nominal", "binary"))
  model      <- match.arg(model)
  thresholds <- match.arg(thresholds)
  irt_require_brms()

  if (type == "nominal" && model == "2pl")
    stop("a 2PL is not supported for nominal items: the nominal response model ",
         "already estimates category-specific slopes. Use model = \"1pl\".",
         call. = FALSE)

  item_thresholds <- type == "ordinal" && thresholds == "item"

  if (model == "1pl") {
    f <- if (item_thresholds) {
      brms::bf(stats::as.formula("response | thres(gr = item) ~ 1 + (1 | .id)"))
    } else {
      brms::bf(stats::as.formula("response ~ 1 + (1 | item) + (1 | .id)"))
    }
    prior <- if (type == "nominal") NULL else {
      p <- c(brms::prior_string("normal(0, 3)", class = "Intercept"),
             brms::prior_string("normal(0, 3)", class = "sd", group = ".id"))
      if (!item_thresholds)
        p <- p + brms::prior_string("normal(0, 3)", class = "sd", group = "item")
      p
    }
  } else if (type == "binary") {
    f <- brms::bf(
      stats::as.formula("response ~ exp(logalpha) * eta"),
      stats::as.formula("eta ~ 1 + (1 |i| item) + (1 | .id)"),
      stats::as.formula("logalpha ~ 1 + (1 |i| item)"),
      nl = TRUE
    )
    prior <- c(
      brms::prior_string("normal(0, 3)", class = "b", nlpar = "eta"),
      brms::prior_string("normal(0, 1)", class = "b", nlpar = "logalpha"),
      brms::prior_string("normal(0, 3)", class = "sd", group = ".id",
                         nlpar = "eta"),
      brms::prior_string("normal(0, 3)", class = "sd", group = "item",
                         nlpar = "eta"),
      brms::prior_string("normal(0, 1)", class = "sd", group = "item",
                         nlpar = "logalpha")
    )
  } else {
    # Ordinal 2PL: the discrimination enters through the cumulative `disc`
    # parameter rather than a non-linear multiplier.
    rhs <- if (item_thresholds) {
      "response | thres(gr = item) ~ 1 + (1 |i| item) + (1 | .id)"
    } else {
      "response ~ 1 + (1 |i| item) + (1 | .id)"
    }
    f <- brms::bf(stats::as.formula(rhs),
                  stats::as.formula("disc ~ 1 + (1 |i| item)"))
    prior <- c(
      brms::prior_string("normal(0, 3)", class = "sd", group = ".id"),
      brms::prior_string("normal(0, 3)", class = "sd", group = "item"),
      brms::prior_string("normal(0, 1)", class = "sd", group = "item",
                         dpar = "disc")
    )
  }

  family <- switch(
    type,
    binary  = brms::bernoulli("logit"),
    ordinal = brms::cumulative("logit"),
    nominal = brms::categorical("logit")
  )

  label <- sprintf(
    "%s %s model%s", toupper(model), type,
    if (type == "ordinal") paste0(" (", thresholds, " thresholds)") else ""
  )

  list(formula = f, family = family, prior = prior, label = label,
       type = type, model = model, thresholds = thresholds)
}

#' @title brms_irt
#' @description Fit a Bayesian item response model with \pkg{brms} and return
#'   everything needed to use and defend the latent scores: the appended data
#'   frame, item parameters, convergence statistics, and graphical diagnostics
#'   as \pkg{ggplot2} objects.
#'
#' @param data A data frame in wide form, one row per respondent.
#' @param items Character vector of at least two columns of `data` measuring the
#'   latent variable.
#' @param type Measurement level of the items: `"ordinal"`, `"nominal"`, or
#'   `"binary"`. Required -- the choice changes the likelihood, so there is no
#'   default to fall back on.
#' @param id Optional column of `data` uniquely identifying respondents. When
#'   `NULL` the row number is used and returned as `.row_id`.
#' @param model `"1pl"` or `"2pl"`; see [irt_formula()].
#' @param thresholds Ordinal items only: `"common"` or `"item"`; see
#'   [irt_formula()].
#' @param score Posterior summary used as each respondent's point estimate:
#'   `"mean"` (default) or `"median"`.
#' @param prefix Prefix for the appended columns. Default `"theta"`, giving
#'   `theta`, `theta_se`, `theta_lower`, `theta_upper`.
#' @param prior Optional \pkg{brms} prior overriding the default from
#'   [irt_formula()].
#' @param chains,iter,warmup,cores,seed Sampler settings passed to
#'   [brms::brm()].
#' @param control Passed to [brms::brm()]'s `control`. Defaults to
#'   `adapt_delta = 0.95`, since hierarchical IRT posteriors are often mildly
#'   funnel-shaped.
#' @param file Optional path (no extension) at which [brms::brm()] caches the
#'   fit, so re-running an analysis does not re-sample.
#' @param loo Whether to add approximate leave-one-out cross-validation.
#'   Default `FALSE`: it is expensive on long-format IRT data.
#' @param n_trace Number of parameters shown in the traceplot panel. Default 8.
#' @param ... Further arguments passed to [brms::brm()].
#'
#' @return An object of class `vrm_irt`: a list with
#'   \describe{
#'     \item{data}{`data` with the latent score, its posterior SD, and a 95%
#'       credible interval appended.}
#'     \item{scores}{One row per respondent: identifier, score, SD, interval.}
#'     \item{items}{Item parameters: difficulty for every model, plus
#'       discrimination for a 2PL.}
#'     \item{fit_stats}{One row: `n_obs`, `n_miss`, `n_persons`, `n_items`,
#'       `n_categories`, `rhat_max`, `ess_bulk_min`, `ess_tail_min`,
#'       `n_divergent`, and (if requested) `elpd_loo` and `looic`.}
#'     \item{diagnostics}{\pkg{ggplot2} objects: `trace`, `rhat`, `ess`, `ppc`.}
#'     \item{fit}{The underlying `brmsfit`.}
#'     \item{spec}{The specification from [irt_formula()].}
#'     \item{long}{The long-format frame actually modelled.}
#'   }
#'
#' @details The latent score is the person-level random intercept: for a 1PL,
#'   \eqn{\theta_p} in \eqn{logit(p) = \beta_0 + \beta_{item} + \theta_p}. It is
#'   summarised from the posterior draws, so `theta_se` is a genuine posterior
#'   standard deviation and can be carried into later models rather than treated
#'   as a known constant.
#'
#'   `n_obs` and `n_miss` are always returned next to the estimates: `n_obs`
#'   counts modelled person-item responses, `n_miss` the person-item cells that
#'   were missing and therefore dropped.
#'
#' @seealso [irt_prepare()], [irt_formula()]
#' @examples
#' \dontrun{
#' set.seed(1)
#' theta <- rnorm(200)
#' d <- data.frame(
#'   pid = seq_along(theta),
#'   i1 = rbinom(200, 1, plogis(theta - 0.5)),
#'   i2 = rbinom(200, 1, plogis(theta)),
#'   i3 = rbinom(200, 1, plogis(theta + 0.5))
#' )
#' out <- brms_irt(d, items = c("i1", "i2", "i3"), type = "binary", id = "pid")
#' out$fit_stats
#' out$diagnostics$trace
#' }
#' @export
brms_irt <- function(data, items, type, id = NULL,
                     model = c("1pl", "2pl"),
                     thresholds = c("common", "item"),
                     score = c("mean", "median"),
                     prefix = "theta", prior = NULL,
                     chains = 4, iter = 2000, warmup = floor(iter / 2),
                     cores = 4, seed = 1234,
                     control = list(adapt_delta = 0.95),
                     file = NULL, loo = FALSE, n_trace = 8, ...) {
  irt_require_brms()
  if (missing(type))
    stop("`type` must be given explicitly: one of \"ordinal\", \"nominal\", ",
         "or \"binary\".", call. = FALSE)
  score <- match.arg(score)

  prep <- irt_prepare(data, items = items, type = type, id = id)
  spec <- irt_formula(prep$type, model = model, thresholds = thresholds)

  fit <- brms::brm(
    formula = spec$formula, data = prep$long, family = spec$family,
    prior = if (is.null(prior)) spec$prior else prior,
    chains = chains, iter = iter, warmup = warmup, cores = cores,
    seed = seed, control = control, file = file, ...
  )

  draws  <- brms::as_draws_df(fit)
  scores <- irt_person_scores(draws, prep, score = score, prefix = prefix)

  keyed <- tibble::as_tibble(data)
  if (identical(prep$id_col, ".row_id")) keyed$.row_id <- seq_len(nrow(keyed))

  res <- list(
    data        = dplyr::left_join(keyed, scores, by = prep$id_col),
    scores      = scores,
    items       = irt_item_pars(draws, prep),
    fit_stats   = irt_fit_stats(fit, draws, prep, loo = loo),
    diagnostics = list(
      trace = irt_trace_plot(draws, n_trace = n_trace),
      rhat  = irt_convergence_plot(draws, what = "rhat"),
      ess   = irt_convergence_plot(draws, what = "ess"),
      ppc   = irt_ppc_plot(fit, prep)
    ),
    fit  = fit,
    spec = spec,
    long = prep$long
  )
  class(res) <- c("vrm_irt", "list")
  res
}

#' @export
print.vrm_irt <- function(x, ...) {
  s <- x$fit_stats
  cat("<vrm_irt>", x$spec$label, "\n")
  cat(sprintf("  %d persons x %d items  |  n_obs = %d, n_miss = %d\n",
              s$n_persons, s$n_items, s$n_obs, s$n_miss))
  cat(sprintf("  max Rhat = %.3f   min bulk ESS = %.0f   divergences = %s\n",
              s$rhat_max, s$ess_bulk_min, format(s$n_divergent)))
  cat("  diagnostics: ", paste(names(x$diagnostics), collapse = ", "), "\n",
      sep = "")
  invisible(x)
}

#' Posterior summaries of the person-level latent scores
#' @param draws A draws data frame from [brms::as_draws_df()].
#' @param prep The list returned by [irt_prepare()].
#' @param score `"mean"` or `"median"`.
#' @param prefix Column-name prefix.
#' @return A tibble with one row per respondent.
#' @keywords internal
#' @noRd
irt_person_scores <- function(draws, prep, score = "mean", prefix = "theta") {
  draws <- as.data.frame(draws)  # subsetting a draws_df drops its metadata
  cols <- grep("^r_\\.id", names(draws), value = TRUE)
  if (!length(cols))
    stop("no person-level random effects found in the fit.", call. = FALSE)
  m <- as.matrix(draws[cols])

  ids <- sub(",.*$", "", sub("^r_\\.id(__[^\\[]+)?\\[", "", cols))
  point <- if (score == "mean") colMeans(m) else apply(m, 2, stats::median)

  out <- tibble::tibble(
    id     = irt_match_id_type(ids, prep$long[[prep$id_col]]),
    point  = as.numeric(point),
    se     = apply(m, 2, stats::sd),
    lower  = apply(m, 2, stats::quantile, 0.025),
    upper  = apply(m, 2, stats::quantile, 0.975)
  )
  names(out) <- c(prep$id_col, prefix, paste0(prefix, c("_se", "_lower", "_upper")))
  out
}

#' Coerce ids pulled out of brms parameter names back to their original class
#' @param x Character ids.
#' @param template The identifier column as it appears in the data.
#' @return `x` coerced to the class of `template`.
#' @keywords internal
#' @noRd
irt_match_id_type <- function(x, template) {
  if (is.factor(template))  return(factor(x, levels = levels(template)))
  if (is.integer(template)) return(as.integer(x))
  if (is.numeric(template)) return(as.numeric(x))
  x
}

#' Item difficulties, and discriminations for a 2PL
#' @param draws A draws data frame.
#' @param prep The list returned by [irt_prepare()].
#' @return A tibble with one row per item parameter.
#' @keywords internal
#' @noRd
irt_item_pars <- function(draws, prep) {
  draws <- as.data.frame(draws)
  cols <- grep("^r_item", names(draws), value = TRUE)
  if (!length(cols))
    return(tibble::tibble(item = factor(), parameter = character(),
                          estimate = numeric(), se = numeric(),
                          lower = numeric(), upper = numeric()))
  m <- as.matrix(draws[cols])
  item <- sub(",.*$", "", sub("^r_item(__[^\\[]+)?\\[", "", cols))
  par  <- ifelse(grepl("logalpha|disc", cols), "discrimination", "difficulty")

  tibble::tibble(
    item      = factor(item, levels = prep$items),
    parameter = par,
    estimate  = colMeans(m),
    se        = apply(m, 2, stats::sd),
    lower     = apply(m, 2, stats::quantile, 0.025),
    upper     = apply(m, 2, stats::quantile, 0.975)
  ) |>
    dplyr::arrange(.data$parameter, .data$item)
}

#' Convergence and sample-size statistics for an IRT fit
#' @param fit A `brmsfit`.
#' @param draws A draws data frame.
#' @param prep The list returned by [irt_prepare()].
#' @param loo Whether to add LOO cross-validation.
#' @return A one-row tibble.
#' @keywords internal
#' @noRd
irt_fit_stats <- function(fit, draws, prep, loo = FALSE) {
  sm  <- posterior::summarise_draws(draws, "rhat", "ess_bulk", "ess_tail")
  np  <- tryCatch(brms::nuts_params(fit), error = function(e) NULL)
  div <- if (is.null(np)) NA_integer_ else
    as.integer(sum(np$Value[np$Parameter == "divergent__"], na.rm = TRUE))

  out <- tibble::tibble(
    n_obs        = prep$n_obs,
    n_miss       = prep$n_miss,
    n_persons    = prep$n_persons,
    n_items      = length(prep$items),
    n_categories = length(prep$levels),
    rhat_max     = suppressWarnings(max(sm$rhat, na.rm = TRUE)),
    ess_bulk_min = suppressWarnings(min(sm$ess_bulk, na.rm = TRUE)),
    ess_tail_min = suppressWarnings(min(sm$ess_tail, na.rm = TRUE)),
    n_divergent  = div
  )
  if (loo) {
    l <- brms::loo(fit)
    out$elpd_loo <- l$estimates["elpd_loo", "Estimate"]
    out$looic    <- l$estimates["looic", "Estimate"]
  }
  out
}

#' Traceplots as a ggplot
#' @param draws A draws data frame.
#' @param n_trace Number of parameters to show.
#' @return A `ggplot`.
#' @keywords internal
#' @noRd
irt_trace_plot <- function(draws, n_trace = 8) {
  meta <- c(".chain", ".iteration", ".draw")
  keep <- grep("^(b_|bs_|sd_|Intercept|disc)", names(draws), value = TRUE)
  if (!length(keep)) keep <- setdiff(names(draws), meta)
  keep <- keep[seq_len(min(n_trace, length(keep)))]

  long <- tidyr::pivot_longer(
    as.data.frame(draws)[c(".chain", ".iteration", keep)],
    cols = dplyr::all_of(keep), names_to = "parameter", values_to = "value"
  )
  ggplot2::ggplot(long, ggplot2::aes(.data$.iteration, .data$value,
                                     colour = factor(.data$.chain))) +
    ggplot2::geom_line(linewidth = 0.25, alpha = 0.8) +
    ggplot2::facet_wrap(~ parameter, scales = "free_y") +
    ggplot2::labs(x = "Iteration", y = NULL, colour = "Chain") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position = "bottom")
}

#' Rhat or effective-sample-size summaries as a ggplot
#' @param draws A draws data frame.
#' @param what `"rhat"` or `"ess"`.
#' @return A `ggplot`.
#' @keywords internal
#' @noRd
irt_convergence_plot <- function(draws, what = c("rhat", "ess")) {
  what <- match.arg(what)
  sm <- posterior::summarise_draws(draws, "rhat", "ess_bulk", "ess_tail")
  sm <- sm[!sm$variable %in% c("lprior", "lp__"), ]

  if (what == "rhat") {
    ggplot2::ggplot(sm, ggplot2::aes(.data$rhat)) +
      ggplot2::geom_histogram(bins = 40, fill = "#2a78d6", colour = "white") +
      ggplot2::geom_vline(xintercept = 1.01, linetype = 2, colour = "#e34948") +
      ggplot2::labs(x = "R-hat", y = "Parameters",
                    subtitle = "Dashed line at 1.01") +
      ggplot2::theme_minimal(base_size = 10)
  } else {
    long <- tidyr::pivot_longer(
      sm[c("variable", "ess_bulk", "ess_tail")],
      cols = c("ess_bulk", "ess_tail"), names_to = "kind", values_to = "ess"
    )
    ggplot2::ggplot(long, ggplot2::aes(.data$ess, fill = .data$kind)) +
      ggplot2::geom_histogram(bins = 40, colour = "white",
                              position = "identity", alpha = 0.6) +
      ggplot2::geom_vline(xintercept = 400, linetype = 2, colour = "#e34948") +
      ggplot2::labs(x = "Effective sample size", y = "Parameters", fill = NULL,
                    subtitle = "Dashed line at 400") +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(legend.position = "bottom")
  }
}

#' Posterior predictive check on the response-category proportions
#' @param fit A `brmsfit`.
#' @param prep The list returned by [irt_prepare()].
#' @param ndraws Number of posterior draws to replicate.
#' @return A `ggplot`.
#' @keywords internal
#' @noRd
irt_ppc_plot <- function(fit, prep, ndraws = 200) {
  y  <- prep$long$response
  lv <- if (is.factor(y)) levels(y) else sort(unique(y))
  # brms returns category indices for factor responses and the raw values for 0/1
  codes <- if (is.factor(y)) seq_along(lv) else lv
  prop  <- function(v) {
    tab <- table(factor(v, levels = codes))
    as.numeric(tab) / sum(tab)
  }

  yrep <- brms::posterior_predict(fit, ndraws = ndraws)
  rep_props <- t(apply(yrep, 1, prop))
  colnames(rep_props) <- as.character(lv)

  reps <- tibble::as_tibble(rep_props) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "category",
                        values_to = "proportion") |>
    dplyr::summarise(
      lower = stats::quantile(.data$proportion, 0.025),
      upper = stats::quantile(.data$proportion, 0.975),
      mid   = stats::median(.data$proportion),
      .by   = "category"
    )
  obs <- tibble::tibble(
    category = as.character(lv),
    proportion = prop(if (is.factor(y)) as.integer(y) else y)
  )

  ggplot2::ggplot(reps, ggplot2::aes(.data$category)) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                            linewidth = 3, colour = "#cfe0f5") +
    ggplot2::geom_point(ggplot2::aes(y = .data$mid), colour = "#2a78d6",
                        size = 2) +
    ggplot2::geom_point(data = obs, ggplot2::aes(y = .data$proportion),
                        colour = "#e34948", size = 2.4, shape = 18) +
    ggplot2::labs(x = "Response category", y = "Proportion",
                  subtitle = paste("Observed (red) against", ndraws,
                                   "posterior replications (blue)")) +
    ggplot2::theme_minimal(base_size = 10)
}

#' Fail early and clearly when the Bayesian stack is not installed
#' @return Invisibly `TRUE`.
#' @keywords internal
#' @noRd
irt_require_brms <- function() {
  for (pkg in c("brms", "posterior")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("package `", pkg, "` is required for item response models. ",
           "Install it with install.packages(\"", pkg, "\").", call. = FALSE)
  }
  invisible(TRUE)
}
