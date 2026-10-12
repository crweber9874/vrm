wide <- data.frame(
  x1 = c(1, 2), x2 = c(3, 4), y_1 = c(5, 6), y_2 = c(7, 8), female = c(0, 1)
)

test_that("panel_longer reshapes to person-wave with integer waves", {
  long <- panel_longer(wide, cols = c(x1:y_2))
  expect_equal(nrow(long), 4)
  expect_setequal(names(long), c("id", "female", "wave", "x", "y"))
  expect_type(long$wave, "integer")
  expect_equal(long$x, c(1, 3, 2, 4))
  expect_equal(long$y, c(5, 7, 6, 8))
  expect_equal(long$female, c(0, 0, 1, 1))
})

test_that("panel_longer handles year suffixes and multi-underscore stems", {
  d <- data.frame(pid = c(10, 11), trust_gov_2012 = 1:2, trust_gov_2016 = 3:4)
  long <- panel_longer(d, cols = starts_with("trust"), id = "pid")
  expect_equal(sort(unique(long$wave)), c(2012L, 2016L))
  expect_true("trust_gov" %in% names(long))
})

test_that("panel_longer validates input", {
  expect_error(panel_longer(wide, cols = female), "do not match")
  expect_error(panel_longer(data.frame(id = 1, x1 = 1), cols = x1), "already has an `id`")
  expect_error(panel_longer(data.frame(id = c(1, 1), x1 = 1:2), cols = x1, id = "id"), "not unique")
})

long <- data.frame(
  id = c(1, 1, 1, 2, 2),
  wave = c(1, 2, 3, 1, 3),
  x = c(10, 11, 12, 20, 22)
)

test_that("panel_lag aligns by wave and respects gaps", {
  out <- panel_lag(long, x)
  expect_equal(out$x_lag, c(NA, 10, 11, NA, NA))
  expect_equal(out$id, long$id)
})

test_that("panel_lag leads, multi-step lags, and unequal wave spacing", {
  expect_equal(panel_lag(long, x, n = -1)$x_lead, c(11, 12, NA, NA, NA))
  expect_equal(panel_lag(long, x, n = 2)$x_lag2, c(NA, NA, 10, NA, 20))
  yrs <- dplyr::mutate(long, wave = c(2012, 2016, 2018, 2012, 2018))
  expect_equal(panel_lag(yrs, x)$x_lag, c(NA, 10, 11, NA, NA))
})

test_that("panel_lag validates input", {
  expect_error(panel_lag(rbind(long, long[1, ]), x), "Duplicate")
  expect_error(panel_lag(long, x, n = 0), "non-zero")
  expect_error(panel_lag(long, x, wave = "t"), "not found")
  expect_error(panel_lag(dplyr::mutate(long, x_lag = 1), x), "already exist")
})

test_that("panel_center decomposes into between and within parts", {
  d <- data.frame(id = c(1, 1, 2, 2), x = c(1, 3, 10, NA))
  out <- panel_center(d, x)
  expect_equal(out$x_between, c(2, 2, 10, 10))
  expect_equal(out$x_within, c(-1, 1, 0, NA))
  expect_equal(out$x_between + out$x_within, d$x)
})

test_that("panel_center grand-centers between component", {
  d <- data.frame(id = c(1, 1, 2, 2), x = c(1, 3, 10, 10))
  out <- panel_center(d, x, grand_center = TRUE)
  expect_equal(unique(out$x_between), c(-4, 4))
  expect_equal(out$x_within, c(-1, 1, 0, 0))
})

test_that("panel_center returns NA for units with no observed values", {
  d <- data.frame(id = c(1, 1), x = c(NA_real_, NA_real_))
  expect_true(all(is.na(panel_center(d, x)$x_between)))
  expect_error(panel_center(data.frame(id = 1, g = "a"), g), "Non-numeric")
})
