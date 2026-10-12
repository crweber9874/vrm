est <- data.frame(
  term  = rep(c("Intercept", "Fraud", "Norms"), 2),
  group = rep(c("Biden", "Trump"), each = 3),
  mean  = c(0.5, 0.10, 0.02, 0.4, -0.05, 0.08),
  lower = c(0.4, 0.02, -0.06, 0.3, -0.12, 0.01),
  upper = c(0.6, 0.18, 0.10, 0.5, 0.02, 0.15)
)

test_that("gg_coef builds a ggplot with detected columns and drops intercepts", {
  p <- gg_coef(est, color = "group")
  expect_s3_class(p, "ggplot")
  expect_false("Intercept" %in% as.character(p$data$term))
  expect_equal(levels(p$data$term), c("Norms", "Fraud"))
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("gg_coef applies labels and reference line toggle", {
  p <- gg_coef(est, labels = c(Fraud = "Fraud cue", Norms = "Norm cue"), ref = NULL)
  expect_setequal(levels(p$data$term), c("Fraud cue", "Norm cue"))
  geoms <- purrr::map_chr(p$layers, ~ class(.x$geom)[1])
  expect_false("GeomHline" %in% geoms)
})

test_that("gg_coef accepts broom-style names", {
  d <- data.frame(term = "b", estimate = 1, conf.low = 0, conf.high = 2)
  expect_s3_class(gg_coef(d), "ggplot")
})

test_that("gg_coef fails loudly", {
  expect_error(gg_coef(list()), "data frame")
  expect_error(gg_coef(data.frame(term = "a", foo = 1)), "detect the estimate")
  expect_error(gg_coef(dplyr::mutate(est, lower = upper + 1)), "exceed")
  expect_error(gg_coef(est, labels = c(Fraud = "F")), "missing term")
  expect_error(gg_coef(est, color = "nope"), "not found")
})

test_that("theme_vrm returns a theme and validates base_size", {
  expect_s3_class(theme_vrm(), "theme")
  expect_error(theme_vrm(base_size = -1), "positive")
})
