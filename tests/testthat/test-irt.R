# Tests for the brms IRT wrapper. The data-side helpers (irt_prepare,
# irt_id_column, irt_strip_labels) need no sampler and are tested exhaustively.
# irt_formula needs brms loaded but samples nothing. One end-to-end fit is run
# last, at a deliberately small size, and skipped when brms is unavailable.

sim_binary <- function(n = 60, seed = 1) {
  set.seed(seed)
  theta <- stats::rnorm(n)
  data.frame(
    pid = paste0("r", seq_len(n)),
    i1 = stats::rbinom(n, 1, stats::plogis(theta - 0.5)),
    i2 = stats::rbinom(n, 1, stats::plogis(theta)),
    i3 = stats::rbinom(n, 1, stats::plogis(theta + 0.5))
  )
}

sim_ordinal <- function(n = 60, seed = 1) {
  set.seed(seed)
  theta <- stats::rnorm(n)
  cut5 <- function(x) as.integer(cut(x, c(-Inf, -1, -0.2, 0.4, 1.2, Inf)))
  data.frame(
    pid = seq_len(n),
    a = cut5(theta + stats::rnorm(n, 0, 0.5)),
    b = cut5(theta + stats::rnorm(n, 0, 0.5)),
    c = cut5(theta + stats::rnorm(n, 0, 0.5))
  )
}

# ---- identifier handling ---------------------------------------------------

test_that("row number is used as the id when none is supplied", {
  d <- sim_binary(10)
  prep <- irt_prepare(d, items = c("i1", "i2", "i3"), type = "binary")
  expect_equal(prep$id_col, ".row_id")
  expect_setequal(as.integer(as.character(prep$long$.row_id)), 1:10)
})

test_that("a supplied id is used and validated", {
  d <- sim_binary(10)
  prep <- irt_prepare(d, items = c("i1", "i2", "i3"), type = "binary",
                      id = "pid")
  expect_equal(prep$id_col, "pid")
  expect_setequal(unique(prep$long$pid), d$pid)

  expect_error(irt_prepare(d, c("i1", "i2"), "binary", id = "nope"),
               "not found")
  expect_error(irt_prepare(d, c("i1", "i2"), "binary", id = c("a", "b")),
               "single column name")

  dup <- d; dup$pid[2] <- dup$pid[1]
  expect_error(irt_prepare(dup, c("i1", "i2"), "binary", id = "pid"),
               "uniquely identify")

  na_id <- d; na_id$pid[3] <- NA
  expect_error(irt_prepare(na_id, c("i1", "i2"), "binary", id = "pid"),
               "missing values")
})

test_that("an existing .row_id column is not silently overwritten", {
  d <- sim_binary(10)
  d$.row_id <- rev(seq_len(10))
  expect_error(irt_prepare(d, c("i1", "i2"), "binary"), "already has")
  prep <- irt_prepare(d, c("i1", "i2"), "binary", id = ".row_id")
  expect_equal(prep$id_col, ".row_id")
})

# ---- type is mandatory and enforced ---------------------------------------

test_that("type must be supplied explicitly", {
  d <- sim_binary(10)
  expect_error(irt_prepare(d, items = c("i1", "i2")), "must be given explicitly")
  expect_error(irt_prepare(d, items = c("i1", "i2"), type = "continuous"))
})

test_that("binary rejects items with more than two categories", {
  d <- sim_ordinal(20)
  expect_error(irt_prepare(d, c("a", "b", "c"), type = "binary"),
               "more than two observed values")
})

test_that("ordinal rejects constant items and warns on two categories", {
  d <- sim_ordinal(20)
  d$a <- 3
  expect_error(irt_prepare(d, c("a", "b", "c"), type = "ordinal"),
               "fewer than two observed values")

  two <- data.frame(x = c(1, 2, 1, 2), y = c(2, 1, 2, 1))
  expect_warning(irt_prepare(two, c("x", "y"), type = "ordinal"),
                 "may be more appropriate")
})

# ---- item validation ------------------------------------------------------

test_that("items must exist, be distinct, and number at least two", {
  d <- sim_binary(10)
  expect_error(irt_prepare(d, "i1", "binary"), "at least two")
  expect_error(irt_prepare(d, c("i1", "i1"), "binary"), "duplicates")
  expect_error(irt_prepare(d, c("i1", "ghost"), "binary"), "not found")
  expect_error(irt_prepare(list(a = 1), c("a", "b"), "binary"), "data frame")
})

test_that("an all-missing item set is an error", {
  d <- data.frame(a = rep(NA_real_, 5), b = rep(NA_real_, 5))
  expect_error(irt_prepare(d, c("a", "b"), "ordinal"), "entirely missing")
})

# ---- reshaping and bookkeeping --------------------------------------------

test_that("n_obs and n_miss account for every person-item cell", {
  d <- sim_ordinal(20)
  d$a[1:3] <- NA
  d$b[5]   <- NA
  prep <- irt_prepare(d, c("a", "b", "c"), type = "ordinal", id = "pid")
  expect_equal(prep$n_miss, 4L)
  expect_equal(prep$n_obs, 20 * 3 - 4)
  expect_equal(nrow(prep$long), prep$n_obs)
  expect_false(anyNA(prep$long$response))
})

test_that("binary responses are coded 0/1 in sorted order", {
  d <- data.frame(x = c(2, 5, 5), y = c(5, 2, 2))
  prep <- irt_prepare(d, c("x", "y"), type = "binary")
  expect_equal(prep$levels, c(2, 5))
  expect_equal(prep$long$response[prep$long$item == "x"], c(0L, 1L, 1L))
  expect_true(is.integer(prep$long$response))
})

test_that("ordinal responses share one ordered scale across items", {
  d <- data.frame(a = c(1, 2, 3), b = c(2, 3, 4))
  prep <- irt_prepare(d, c("a", "b"), type = "ordinal")
  expect_true(is.ordered(prep$long$response))
  expect_equal(levels(prep$long$response), as.character(1:4))
})

test_that("nominal responses are unordered factors", {
  d <- data.frame(a = c("yes", "no", "maybe"), b = c("no", "no", "yes"))
  prep <- irt_prepare(d, c("a", "b"), type = "nominal")
  expect_true(is.factor(prep$long$response))
  expect_false(is.ordered(prep$long$response))
  expect_setequal(levels(prep$long$response), c("maybe", "no", "yes"))
})

test_that("item order is preserved and labelled vectors are tolerated", {
  d <- sim_ordinal(10)
  attr(d$a, "label") <- "an item label"
  class(d$b) <- c("haven_labelled", "double")
  prep <- irt_prepare(d, c("c", "a", "b"), type = "ordinal", id = "pid")
  expect_equal(levels(prep$long$item), c("c", "a", "b"))
  expect_equal(prep$items, c("c", "a", "b"))
})

test_that("persons contributing no responses are dropped from the long frame", {
  d <- sim_ordinal(10)
  d[4, c("a", "b", "c")] <- NA
  prep <- irt_prepare(d, c("a", "b", "c"), type = "ordinal", id = "pid")
  expect_equal(prep$n_persons, 9L)
  expect_false("4" %in% as.character(prep$long$pid))
})

# ---- internal helpers -----------------------------------------------------

test_that("irt_strip_labels removes attributes and flattens factors", {
  x <- structure(c(1, 2), label = "lab", class = c("haven_labelled", "double"))
  expect_null(attributes(vrm:::irt_strip_labels(x)))
  expect_equal(vrm:::irt_strip_labels(factor(c("b", "a"))), c("b", "a"))
})

test_that("irt_match_id_type restores the original id class", {
  expect_type(vrm:::irt_match_id_type(c("1", "2"), 1L:2L), "integer")
  expect_type(vrm:::irt_match_id_type(c("1", "2"), c(1.5, 2.5)), "double")
  expect_type(vrm:::irt_match_id_type(c("a", "b"), c("a", "b")), "character")
  f <- factor(c("a", "b"))
  expect_s3_class(vrm:::irt_match_id_type(c("a", "b"), f), "factor")
})

# ---- model specification --------------------------------------------------

test_that("irt_formula builds the expected family per type", {
  skip_if_not_installed("brms")
  expect_equal(irt_formula("binary")$family$family, "bernoulli")
  expect_equal(irt_formula("ordinal")$family$family, "cumulative")
  expect_equal(irt_formula("nominal")$family$family, "categorical")
})

test_that("irt_formula puts item-specific thresholds in the formula", {
  skip_if_not_installed("brms")
  common <- irt_formula("ordinal", thresholds = "common")
  item   <- irt_formula("ordinal", thresholds = "item")
  expect_false(grepl("thres", deparse1(common$formula$formula)))
  expect_true(grepl("thres", deparse1(item$formula$formula)))
  # item thresholds absorb item difficulty, so the item intercept is dropped
  expect_false(grepl("1 \\| item", deparse1(item$formula$formula)))
  expect_true(grepl("1 \\| item", deparse1(common$formula$formula)))
})

test_that("a 2PL is non-linear for binary and uses disc for ordinal", {
  skip_if_not_installed("brms")
  b <- irt_formula("binary", model = "2pl")
  expect_true(isTRUE(attr(b$formula$formula, "nl")))
  expect_true(grepl("logalpha", deparse1(b$formula$formula)))

  o <- irt_formula("ordinal", model = "2pl")
  expect_true("disc" %in% names(o$formula$pforms))
})

test_that("a 2PL is refused for nominal items", {
  skip_if_not_installed("brms")
  expect_error(irt_formula("nominal", model = "2pl"), "not supported")
})

test_that("irt_formula validates its own arguments", {
  skip_if_not_installed("brms")
  expect_error(irt_formula(), "must be given explicitly")
  expect_error(irt_formula("ordinal", model = "3pl"))
  expect_error(irt_formula("ordinal", thresholds = "free"))
})

test_that("labels name the model that was built", {
  skip_if_not_installed("brms")
  expect_match(irt_formula("ordinal", "1pl", "item")$label,
               "1PL ordinal model \\(item thresholds\\)")
  expect_match(irt_formula("binary", "2pl")$label, "2PL binary model")
})

# ---- end to end -----------------------------------------------------------

test_that("brms_irt returns scores, items, stats and plots", {
  skip_on_cran()
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  skip_if_not(isTRUE(as.logical(Sys.getenv("VRM_TEST_STAN", "false"))),
              "set VRM_TEST_STAN=true to run models that compile Stan code")

  d <- sim_binary(60)
  out <- brms_irt(d, items = c("i1", "i2", "i3"), type = "binary", id = "pid",
                  chains = 2, iter = 600, cores = 1, seed = 2,
                  refresh = 0, silent = 2)

  expect_s3_class(out, "vrm_irt")
  expect_equal(nrow(out$scores), 60L)
  expect_true(all(c("theta", "theta_se", "theta_lower", "theta_upper") %in%
                    names(out$data)))
  expect_equal(nrow(out$data), nrow(d))
  expect_false(anyNA(out$data$theta))
  expect_true(all(out$data$theta_lower <= out$data$theta))
  expect_true(all(out$data$theta >= -10 & out$data$theta <= 10))

  expect_equal(out$fit_stats$n_obs, 180L)
  expect_equal(out$fit_stats$n_miss, 0L)
  expect_equal(out$fit_stats$n_persons, 60L)
  expect_equal(out$fit_stats$n_items, 3L)

  expect_setequal(as.character(out$items$item), c("i1", "i2", "i3"))
  expect_setequal(out$items$parameter, "difficulty")

  expect_setequal(names(out$diagnostics), c("trace", "rhat", "ess", "ppc"))
  for (p in out$diagnostics) expect_s3_class(p, "ggplot")

  # the recovered scores should track the generating latent variable
  set.seed(1); theta_true <- stats::rnorm(60)
  expect_gt(stats::cor(out$data$theta, theta_true), 0.5)

  expect_output(print(out), "vrm_irt")
})

test_that("missing responses are dropped but still counted end to end", {
  skip_on_cran()
  skip_if_not_installed("brms")
  skip_if_not(isTRUE(as.logical(Sys.getenv("VRM_TEST_STAN", "false"))),
              "set VRM_TEST_STAN=true to run models that compile Stan code")

  d <- sim_ordinal(50)
  d$a[1:5] <- NA
  out <- brms_irt(d, items = c("a", "b", "c"), type = "ordinal", id = "pid",
                  chains = 2, iter = 600, cores = 1, seed = 3,
                  refresh = 0, silent = 2)
  expect_equal(out$fit_stats$n_miss, 5L)
  expect_equal(out$fit_stats$n_obs, 50 * 3 - 5)
  expect_equal(nrow(out$data), 50L)
})
