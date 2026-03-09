#' Fit a battery of brms models across items, families, and weighting approaches
#'
#' Loops over combinations of outcome items, model families, and
#' optionally weighted/unweighted variants, fitting a \pkg{brms} model
#' for each. Models are cached to disk via \code{brms::brm(file = ...)}.
#'
#' @param data A data.frame containing all variables.
#' @param items Character vector of outcome column names.
#' @param rhs Character string for the right-hand side of the formula
#'   (e.g., \code{"treatment * moderator + covariate"}).
#' @param families Named list of brms family objects
#'   (e.g., \code{list(ord = cumulative(), linear = gaussian())}).
#' @param data_dir Directory path for caching model files.
#' @param prefix File name prefix for cached models.
#' @param weight_var Column name for survey weights, or \code{NULL} to skip
#'   weighted models. Default \code{"wts"}.
#' @param cores Number of cores for parallel chains. Default 10.
#' @param chains Number of MCMC chains. Default 3.
#' @param iter Total iterations per chain. Default 2000.
#' @param control List of Stan control parameters.
#' @return A named list of \code{brmsfit} objects.
#' @export
fit_brms_battery <- function(data,
                                    items,
                                    rhs,
                                    families,
                                    data_dir,
                                    prefix,
                                    weight_var = "wts",
                                    cores = 10,
                                    chains = 3,
                                    iter = 2000,
                                    control = list(max_treedepth = 15)) {
  combs <- tidyr::expand_grid(
    item = items,
    family = names(families),
    weighted = if (!is.null(weight_var)) c(FALSE, TRUE) else FALSE
  )

  model_names <- combs %>%
    dplyr::transmute(name = paste0(item, "_", family,
                                   dplyr::if_else(weighted, "_w", ""))) %>%
    dplyr::pull(name)

  model_list <- vector("list", length(model_names))
  names(model_list) <- model_names

  for (i in seq_len(nrow(combs))) {
    row <- combs[i, ]
    current_name <- model_names[i]
    formula_str <- if (isTRUE(row$weighted)) {
      if (is.null(weight_var) || !rlang::has_name(data, weight_var)) {
        stop("Weight variable is missing from the data but weighted models requested.")
      }
      paste0(row$item, " | weights(", weight_var, ") ~ ", rhs)
    } else {
      paste0(row$item, " ~ ", rhs)
    }

    model_list[[i]] <- brms::brm(
      brms::bf(formula_str),
      data = data,
      family = families[[row$family]],
      cores = cores,
      chains = chains,
      iter = iter,
      control = control,
      file = file.path(data_dir, paste0(prefix, "_", current_name)),
      file_refit = "on_change"
    )
  }

  model_list
}

#' Clean brms coefficient term names
#'
#' Strips \code{b_} and \code{mu\\d+_} prefixes from brms parameter names.
#'
#' @param term Character vector of term names from \code{brms::fixef()}.
#' @return Character vector with prefixes removed.
#' @export
clean_term_name <- function(term) {
  term %>%
    stringr::str_remove("^b_") %>%
    stringr::str_remove("^mu\\d+_")
}

#' Parse model metadata from naming convention
#'
#' Extracts item name, family code, and weighted status from a model name
#' following the convention \code{item_family[_w]}.
#'
#' @param name Character string model name.
#' @return A one-row tibble with columns \code{item}, \code{family}, \code{weighted}.
#' @export
parse_model_metadata <- function(name) {
  fam <- stringr::str_match(name, "_(ord|mlogit|linear)")[, 2]
  tibble::tibble(
    item = stringr::str_remove(name, paste0("_", fam, "(_w)?$")),
    family = fam,
    weighted = stringr::str_detect(name, "_w$")
  )
}

#' Default term label mapping (project-specific convenience)
#'
#' Returns a named character vector mapping model terms to display labels.
#' This is a project-specific default for pre/post x treatment designs.
#' Override via the \code{term_labels} parameter of \code{brms_regression_table()}.
#'
#' @return Named character vector.
#' @export
default_term_map <- function() {
  c(
    "prepost" = "Post (vs. Pre)",
    "vote_trump" = "Trump Voter",
    "prepost:vote_trump" = "Post × Trump"
  )
}

#' Default model family display names
#'
#' Maps family codes to human-readable names for tables.
#'
#' @return Named character vector.
#' @export
default_model_lookup <- function() {
  c(ord = "Ordinal Logit",
    mlogit = "Multinomial Logit",
    linear = "Linear Gaussian")
}

#' Extract tidy fixed effects from a brms model
#'
#' @param model A \code{brmsfit} object.
#' @return A tibble with columns \code{term}, \code{estimate}, \code{std.error},
#'   \code{conf.low}, \code{conf.high}.
#' @export
tidy_fixed_effects <- function(model) {
  fx <- brms::fixef(model, robust = FALSE)
  tibble::tibble(
    term = rownames(fx),
    estimate = fx[, "Estimate"],
    std.error = fx[, "Est.Error"],
    conf.low = fx[, "Q2.5"],
    conf.high = fx[, "Q97.5"]
  )
}

#' Build a publication-ready regression table from a list of brms models
#'
#' Extracts fixed effects from each model, maps terms to display labels,
#' and formats a \pkg{gt} table grouped by outcome item.
#'
#' @param model_list Named list of \code{brmsfit} objects (names follow
#'   \code{item_family[_w]} convention).
#' @param dataset_label Character string for the table title.
#' @param level_lookup Named list mapping items to response level labels.
#' @param term_labels Named character vector mapping term names to display
#'   labels. Default uses \code{default_term_map()}.
#' @param model_lookup Named character vector mapping family codes to display
#'   names. Default uses \code{default_model_lookup()}.
#' @return A \code{gt} table object.
#' @export
brms_regression_table <- function(model_list,
                                          dataset_label,
                                          level_lookup,
                                          term_labels = default_term_map(),
                                          model_lookup = default_model_lookup()) {
  model_levels <- unname(model_lookup)
  term_order <- unname(term_labels)

  rows <- purrr::map_dfr(names(model_list), function(nm) {
    meta <- parse_model_metadata(nm)
    lvls <- level_lookup[[meta$item]]
    if (is.null(lvls)) lvls <- character()

    tidy_fixed_effects(model_list[[nm]]) %>%
      dplyr::mutate(
        term_clean = clean_term_name(.data$term),
        category_idx = as.integer(stringr::str_match(.data$term, "mu(\\d+)_")[, 2]),
        # Map mu index columns back to their factor level labels when present.
        category_raw = {
          idx <- .data$category_idx
          out <- rep(NA_character_, length(idx))
          if (length(lvls) > 0) {
            valid <- !is.na(idx) & idx <= length(lvls)
            out[valid] <- lvls[idx[valid]]
          }
          out
        }
      ) %>%
      dplyr::filter(.data$term_clean %in% names(term_labels)) %>%
      dplyr::mutate(
        Item = stringr::str_to_title(stringr::str_replace_all(meta$item, "_", " ")),
        Model = model_lookup[[meta$family]],
        Weighting = dplyr::if_else(meta$weighted, "IPTW", "Unweighted"),
        Term = term_labels[.data$term_clean],
        Response = dplyr::if_else(
          Model == "Multinomial Logit",
          dplyr::coalesce(as.character(.data$category_raw), "Non-Reference"),
          ""
        )
      ) %>%
      dplyr::select(Item, Model, Weighting, Response, Term,
                    estimate, std.error, conf.low, conf.high)
  })

  if (nrow(rows) == 0) {
    return(gt::gt(tibble::tibble(Message = "No coefficients to display")))
  }

  rows %>%
    dplyr::mutate(
      Model = factor(Model, levels = model_levels),
      Weighting = factor(Weighting, levels = c("Unweighted", "IPTW")),
      Term = factor(Term, levels = term_order)
    ) %>%
    dplyr::arrange(Item, Model, Weighting, Term) %>%
    gt::gt(groupname_col = "Item") %>%
    gt::tab_header(title = dataset_label,
                   subtitle = "Posterior means and 95% credible intervals") %>%
    gt::cols_label(Model = "Model",
                   Weighting = "Weighting",
                   Response = "Response",
                   Term = "Coefficient",
                   estimate = "Estimate",
                   std.error = "SE",
                   conf.low = "95% Lower",
                   conf.high = "95% Upper") %>%
    gt::fmt_number(columns = c(estimate, std.error, conf.low, conf.high), decimals = 3) %>%
    gt::opt_row_striping() %>%
    gt::tab_options(table.font.size = "small", data_row.padding = gt::px(2))
}

#' Summarize MCMC diagnostics for a list of brms models
#'
#' Extracts divergences, max treedepth warnings, Rhat, and ESS ratio
#' for each model and flags any with potential issues.
#'
#' @param model_list Named list of \code{brmsfit} objects.
#' @return A tibble with diagnostic metrics and a \code{flag} column.
#' @export
summarize_brm_diagnostics <- function(model_list) {
  tibble::tibble(model = names(model_list)) %>%
    dplyr::mutate(metrics = purrr::map(model_list, function(m) {
      np <- brms::nuts_params(m)
      tibble::tibble(
        n_div = sum(subset(np, Parameter == "divergent__")$Value),
        n_tree = sum(subset(np, Parameter == "treedepth__")$Value >= 15),
        max_rhat = max(brms::rhat(m), na.rm = TRUE),
        min_ess = min(brms::neff_ratio(m), na.rm = TRUE)
      )
    })) %>%
    tidyr::unnest(metrics) %>%
    dplyr::mutate(flag = n_div > 0 | n_tree > 0 | max_rhat > 1.05 | min_ess < 0.1)
}

#' Print human-readable diagnostic warnings
#'
#' @param diag_tbl Tibble from \code{summarize_brm_diagnostics()}.
#' @param label Character label for the diagnostic output header.
#' @return Invisibly returns the input tibble.
#' @export
print_diagnostic_warnings <- function(diag_tbl, label) {
  flagged <- diag_tbl %>% dplyr::filter(flag)
  if (nrow(flagged) == 0) {
    message(label, ": all models passed basic diagnostics.")
    return(invisible(diag_tbl))
  }
  message("--- ", label, " Model Diagnostics ---")
  purrr::pwalk(flagged, function(model, n_div, n_tree, max_rhat, min_ess, flag) {
    issues <- c()
    if (n_div > 0) issues <- c(issues, paste0(n_div, " divergent transitions"))
    if (n_tree > 0) issues <- c(issues, paste0(n_tree, " exceeded max treedepth"))
    if (max_rhat > 1.05) issues <- c(issues, paste0("max Rhat = ", round(max_rhat, 3)))
    if (min_ess < 0.1) issues <- c(issues, paste0("min ESS ratio = ", round(min_ess, 3)))
    message("  WARNING ", model, ": ", paste(issues, collapse = "; "))
  })
  invisible(diag_tbl)
}


#' Compute marginal effects of pre-to-post across vote groups from a model battery
#'
#' For each item in a fitted model battery, computes the marginal effect of
#' moving from pre to post election, separately for Trump and non-Trump voters.
#' Handles linear (difference in means), ordinal (odds ratio), and multinomial
#' (probability contrast for a focal category) models.
#'
#' @param model_list Named list of \code{brmsfit} objects from \code{fit_brms_battery()}.
#' @param items Character vector of item names.
#' @param treatment Character. Treatment variable name. Default \code{"prepost"}.
#' @param moderator Character. Moderator variable name. Default \code{"vote_trump"}.
#' @param focal_category Character. For multinomial models, which response category
#'   to contrast. Default \code{"Strongly Support"}.
#' @param response_levels Character vector giving the ordered response labels for
#'   contestation items. Used to align \code{focal_category} with categorical
#'   predictions even when the stored factor levels differ. Defaults to the
#'   five-point contestation scale.
#' @param prob Numeric. Credible interval width. Default 0.95.
#' @return A tibble with columns: \code{item}, \code{family}, \code{weighted},
#'   \code{voter}, \code{effect}, \code{lower}, \code{upper}, \code{effect_type}.
#' @export
marginal_effects_battery <- function(model_list,
                                     items,
                                     treatment = "prepost",
                                     moderator = "vote_trump",
                                     focal_category = "Strongly Support",
                                     response_levels = c(
                                       "Strongly Oppose",
                                       "Oppose",
                                       "Neither",
                                       "Support",
                                       "Strongly Support"
                                     ),
                                     families = NULL,
                                     prob = 0.95) {
  alpha <- (1 - prob) / 2
  probs <- c(alpha, 1 - alpha)
  sanitize_label <- function(x) {
    stringr::str_replace_all(stringr::str_to_lower(trimws(x)), "[^a-z0-9]+", "")
  }

  results <- purrr::map_dfr(names(model_list), function(nm) {
    meta <- parse_model_metadata(nm)
    if (!(meta$item %in% items)) return(NULL)
    if (!is.null(families) && !(meta$family %in% families)) return(NULL)

    model <- model_list[[nm]]
    fam <- meta$family

    if (fam == "linear") {
      # Extract coefficients directly — no need for add_epred_draws
      post_draws <- brms::as_draws_df(model)
      b_treat <- post_draws[["b_prepost"]]
      b_inter <- post_draws[["b_prepost:vote_trump"]]

      me_non_trump <- b_treat
      me_trump <- b_treat + b_inter

      me <- dplyr::bind_rows(
        tibble::tibble(
          !!moderator := 0,
          effect = mean(me_non_trump),
          lower = stats::quantile(me_non_trump, probs[1]),
          upper = stats::quantile(me_non_trump, probs[2])
        ),
        tibble::tibble(
          !!moderator := 1,
          effect = mean(me_trump),
          lower = stats::quantile(me_trump, probs[1]),
          upper = stats::quantile(me_trump, probs[2])
        )
      ) %>%
        dplyr::mutate(effect_type = "Mean Difference")

    } else if (fam == "ord") {
      # Odds ratio: extract posterior draws of coefficients directly
      post_draws <- brms::as_draws_df(model)
      # treatment effect for non-Trump (beta_prepost) and Trump (beta_prepost + beta_interaction)
      b_treat <- post_draws[["b_prepost"]]
      b_inter <- post_draws[["b_prepost:vote_trump"]]

      me_non_trump <- exp(b_treat)
      me_trump <- exp(b_treat + b_inter)

      me <- dplyr::bind_rows(
        tibble::tibble(
          !!moderator := 0,
          effect = mean(me_non_trump),
          lower = stats::quantile(me_non_trump, probs[1]),
          upper = stats::quantile(me_non_trump, probs[2])
        ),
        tibble::tibble(
          !!moderator := 1,
          effect = mean(me_trump),
          lower = stats::quantile(me_trump, probs[1]),
          upper = stats::quantile(me_trump, probs[2])
        )
      ) %>%
        dplyr::mutate(effect_type = "Odds Ratio")

    } else if (fam == "mlogit") {
      # Need posterior predictions for category probabilities
      data <- model$data
      resp_var <- as.character(model$formula$formula[[2]])
      covars <- setdiff(names(data), c(resp_var, treatment, moderator))
      newdata <- data %>%
        dplyr::select(dplyr::all_of(covars)) %>%
        dplyr::summarize(dplyr::across(
          tidyselect::where(is.numeric), mean
        ), dplyr::across(
          tidyselect::where(~ is.factor(.) || is.character(.)),
          ~ names(sort(table(.), decreasing = TRUE))[1]
        )) %>%
        tidyr::expand_grid(
          !!treatment := c(0, 1),
          !!moderator := c(0, 1)
        )
      draws <- tidybayes::add_epred_draws(newdata, model)

      # Probability contrast for focal category
      available_categories <- unique(stats::na.omit(draws$.category))
      levels_ref <- response_levels
      if (length(levels_ref) == 0) levels_ref <- available_categories

      target_category <- focal_category
      display_label <- focal_category

      if (!(target_category %in% available_categories)) {
        normalized_levels <- sanitize_label(levels_ref)
        normalized_levels <- normalized_levels[seq_along(available_categories)]
        match_idx <- match(sanitize_label(focal_category), normalized_levels)

        if (is.na(match_idx)) {
          numeric_idx <- suppressWarnings(as.integer(focal_category))
          if (!is.na(numeric_idx) && numeric_idx >= 1 &&
                numeric_idx <= length(available_categories)) {
            match_idx <- numeric_idx
          }
        }

        if (is.na(match_idx)) {
          match_idx <- length(available_categories)
        }

        target_category <- available_categories[match_idx]
        if (match_idx <= length(levels_ref)) {
          display_label <- levels_ref[match_idx]
        } else {
          display_label <- target_category
        }
      } else {
        idx <- match(target_category, available_categories)
        if (!is.na(idx) && idx <= length(levels_ref)) {
          display_label <- levels_ref[idx]
        }
      }

      me <- draws %>%
        dplyr::filter(.category == target_category) %>%
        dplyr::group_by(!!rlang::sym(moderator), .draw) %>%
        dplyr::summarize(
          me = .epred[!!rlang::sym(treatment) == 1] - .epred[!!rlang::sym(treatment) == 0],
          .groups = "drop"
        ) %>%
        dplyr::group_by(!!rlang::sym(moderator)) %>%
        dplyr::summarize(
          effect = mean(me),
          lower = stats::quantile(me, probs[1]),
          upper = stats::quantile(me, probs[2]),
          .groups = "drop"
        ) %>%
        dplyr::mutate(effect_type = paste0("Pr(", display_label, ") Difference"))
    } else {
      return(NULL)
    }

    me %>%
      dplyr::mutate(
        item = meta$item,
        family = fam,
        weighted = meta$weighted,
        voter = dplyr::if_else(!!rlang::sym(moderator) == 1, "Trump", "Non-Trump")
      ) %>%
      dplyr::select(item, family, weighted, voter, effect, lower, upper, effect_type)
  })

  results
}


#' Plot marginal effects from a marginal effects battery
#'
#' Creates a forest-style plot of marginal effects, faceted by model family.
#'
#' @param me_data Tibble from \code{marginal_effects_battery()}.
#' @param title Character. Plot title.
#' @param colors Named character vector of colors for voter groups.
#' @param dodge_width Numeric. Dodge width for overlapping points. Default 0.4.
#' @return A \code{ggplot} object.
#' @export
plot_marginal_effects <- function(me_data,
                                  title = "Marginal Effect of Post-Election Period",
                                  colors = c("Trump" = "#E63946", "Non-Trump" = "#457B9D"),
                                  dodge_width = 0.6) {
  effect_label <- unique(me_data$effect_type)[1]
  baseline <- ifelse(grepl("Odds Ratio", effect_label), 1, 0)

  me_data %>%
    dplyr::mutate(
      item_label = stringr::str_to_title(stringr::str_replace_all(item, "_", " ")),
      weight_label = dplyr::if_else(weighted, "IPTW", "Unweighted")
    ) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = item_label, y = effect, ymin = lower, ymax = upper,
      color = voter, shape = weight_label
    )) +
    ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = dodge_width), size = 0.7) +
    ggplot2::geom_hline(yintercept = baseline, linetype = "dashed", color = "grey40") +
    ggplot2::coord_flip() +
    ggplot2::scale_color_manual("", values = colors) +
    ggplot2::scale_shape_manual("", values = c("Unweighted" = 16, "IPTW" = 17)) +
    ggplot2::labs(title = title, x = "", y = effect_label) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")
}
