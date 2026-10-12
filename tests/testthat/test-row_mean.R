test_that("row_mean returns NA, not NaN, for all-missing rows", {
  out <- row_mean(c(1, NA), c(3, NA))
  expect_equal(out, c(2, NA_real_))
  expect_false(any(is.nan(out)))
})

test_that("row_mean enforces min_valid as count and proportion", {
  a <- c(1, 2, NA); b <- c(3, NA, NA); c <- c(5, 4, 6)
  expect_equal(row_mean(a, b, c, min_valid = 2), c(3, 3, NA))
  expect_equal(row_mean(a, b, c, min_valid = 1), c(3, 3, 6))
  expect_equal(row_mean(a, b, c, min_valid = 0.5), c(3, 3, NA))
})

test_that("row_mean accepts a data frame and works inside mutate", {
  df <- data.frame(a = c(1, NA), b = c(3, 4))
  expect_equal(row_mean(df), c(2, 4))
  out <- dplyr::mutate(df, s = row_mean(dplyr::pick(a, b)))
  expect_equal(out$s, c(2, 4))
})

test_that("row_mean fails loudly on bad input", {
  expect_error(row_mean(), "at least one")
  expect_error(row_mean(1:3, 1:2), "same length")
  expect_error(row_mean(c("a", "b")), "numeric")
  expect_error(row_mean(1:3, 1:3, min_valid = 3), "exceeds")
  expect_error(row_mean(1:3, min_valid = -1), "positive")
})
