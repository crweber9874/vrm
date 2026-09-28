test_that("clean_term_name strips brms prefixes", {
  expect_equal(clean_term_name("b_prepost"), "prepost")
  expect_equal(clean_term_name("b_mu3_vote_trump"), "vote_trump")  # both prefixes stripped
  expect_equal(clean_term_name("mu2_prepost"), "prepost")
  expect_equal(clean_term_name("Intercept"), "Intercept")
})

test_that("parse_model_metadata splits item, family and weighting", {
  m <- parse_model_metadata("attend_march_ord_w")
  expect_equal(m$item, "attend_march")
  expect_equal(m$family, "ord")
  expect_true(m$weighted)

  m2 <- parse_model_metadata("burn_flag_linear")
  expect_equal(m2$item, "burn_flag")
  expect_equal(m2$family, "linear")
  expect_false(m2$weighted)
})

test_that("parse_model_metadata keeps multi-word items intact", {
  m <- parse_model_metadata("criticize_election_mlogit_w")
  expect_equal(m$item, "criticize_election")
  expect_equal(m$family, "mlogit")
  expect_true(m$weighted)
})

test_that("default lookups cover the three fitted families", {
  expect_setequal(names(default_model_lookup()), c("ord", "mlogit", "linear"))
  expect_true(all(c("prepost", "vote_trump") %in% names(default_term_map())))
})
