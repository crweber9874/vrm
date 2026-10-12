test_that("summarize_predictions handles tidybayes draws", {
  set.seed(1)
  d <- tidyr::expand_grid(.row = 1:2, .draw = 1:200) |>
    dplyr::mutate(.chain = 1L, .iteration = .draw, x = .row,
                  .epred = rnorm(dplyr::n(), mean = x))
  s <- summarize_predictions(d)
  expect_equal(nrow(s), 2)
  expect_equal(names(s)[1], "x")
  expect_equal(s$mean_prob, as.numeric(tapply(d$.epred, d$x, mean)))
  expect_true(all(s$upper_ci > s$lower_ci))
})

test_that("summarize_predictions unchanged for vrm draw format", {
  d <- tidyr::expand_grid(pid = c("D", "R"), draw = 1:100) |>
    dplyr::mutate(predicted_prob = draw / 100)
  s <- summarize_predictions(d)
  expect_equal(nrow(s), 2)
  expect_equal(s$mean_prob, c(0.505, 0.505))
})

test_that("summarize_treatment_effects works without dplyr attached", {
  d <- tidyr::expand_grid(.draw = 1:50, id = 1:10) |>
    dplyr::mutate(treatment_effect = .draw / 50, g = id %% 2)
  s <- summarize_treatment_effects(d, group_vars = "g")
  expect_equal(nrow(s), 2)
  expect_equal(s$mean_effect, rep(mean((1:50) / 50), 2))
})

test_that("zero_one supports theoretical bounds", {
  expect_equal(zero_one(c(10, 20, 30)), c(0, 0.5, 1))
  expect_equal(zero_one(c(2, 3, 4), lo = 1, hi = 5), c(0.25, 0.5, 0.75))
  expect_equal(zero_one(c(1, NA, 3)), c(0, NA, 1))
  expect_warning(r <- zero_one(c(2, 2)), "no range")
  expect_true(all(is.na(r)))
  expect_warning(zero_one(c(0, 6), lo = 1, hi = 5), "outside")
  expect_error(zero_one(1:3, lo = 5, hi = 1), "greater")
  expect_error(zero_one("a"), "numeric")
  expect_equal(zero.one(c(10, 20, 30)), c(0, 0.5, 1))
})

test_that("gg_posterior_ridges accepts a value column", {
  skip_if_not_installed("ggridges")
  d <- tidyr::expand_grid(item = c("a", "b"), grp = c("0", "1"), i = 1:50) |>
    dplyr::mutate(treatment_effect = rnorm(dplyr::n()))
  p <- gg_posterior_ridges(d, "item", "grp", value = "treatment_effect")
  expect_s3_class(p, "ggplot")
  expect_error(gg_posterior_ridges(d, "item", "grp"), ".epred")
})
