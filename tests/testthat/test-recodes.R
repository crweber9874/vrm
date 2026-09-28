test_that("zero_one rescales to the unit interval", {
  expect_equal(zero_one(c(1, 2, 5)), c(0, 0.25, 1))
  expect_equal(range(zero_one(c(-3, 0, 7))), c(0, 1))
})

test_that("zero_one propagates NA without shifting the scale", {
  out <- zero_one(c(1, 2, NA, 5))
  expect_true(is.na(out[3]))
  expect_equal(out[-3], c(0, 0.25, 1))
})

test_that("zero.one is an exact alias for zero_one", {
  x <- c(2, 4, 9, NA)
  expect_equal(zero.one(x), zero_one(x, na.rm = TRUE))
})

test_that("constant input yields NaN rather than an error", {
  expect_true(all(is.nan(zero_one(c(3, 3, 3)))))
})

test_that("five_r reverses and five_n preserves a 5-point scale", {
  expect_equal(unname(five_r[as.character(1:5)]), 5:1)
  expect_equal(unname(five_n[as.character(1:5)]), 1:5)
  expect_equal(unname(four_r[as.character(1:4)]), 4:1)
  expect_equal(unname(four_n[as.character(1:4)]), 1:4)
})

test_that("recodeList applies a rule and adds the new column", {
  df <- data.frame(q1 = c(1, 5, 3))
  out <- recodeList(df, list(list(column = "q1", recode_rules = five_r,
                                  new_column = "q1_rev")))
  expect_true("q1_rev" %in% names(out))
  expect_equal(out$q1_rev, c(5, 1, 3))
  expect_equal(out$q1, c(1, 5, 3))  # source column untouched
})
