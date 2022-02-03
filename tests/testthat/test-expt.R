test_that("make_expt makes a valid experiment", {
  x <- make_expt(ncage_treatment = 3, mice_per_cage_treatment = 2)
  expect_equal(x$number_of_cages_trt, 3)
  expect_equal(x$mice_per_cage_trt, 2)
})

test_that("get_power estimates the power for an experiment, no random effects", {
  expt <- make_expt(ncage_treatment = 10, mice_per_cage_treatment = 1)
  #pwr <- get_power_foreach(expt, d = c(0.9, 1.2, 1.5), nsim = 10, seed = 42)
  pwr <- get_power(expt, d = c(0.9, 1.2, 1.5), p = 0, nsim = 10, seed = 42)
  expect_equal(nrow(pwr), 3)
  expect_equal(pwr$d, c(0.9, 1.2, 1.5))
  # estimate from t-test: c(0.5, 0.7, 0.9)
  expect_equal(pwr$power, c(0.8, 1, 0.7))
  expect_equal(pwr$t_test_power, c(0.478050413, 0.718405377, 0.886970202))
})

test_that("get_power estimates the power for an experiment with random effects", {
  expt <- make_expt(ncage_treatment = 5, mice_per_cage_treatment = 3)
  pwr <- get_power(expt, d = c(1.0, 1.4, 1.8, 2.2, 2.6, 3.0),
                          p = 0.3, nsim = 10, seed = 42)
  expect_equal(nrow(pwr), 6)
  expect_equal(pwr$d, c(1.0, 1.4, 1.8, 2.2, 2.6, 3.0))
  expect_equal(pwr$power, c(0.4, 0.6, 0.9, 1, 1, 1))
  # estimate from t-test: c(0.5, 0.7, 0.9)
  expect_equal(pwr$t_test_power, c(0.286295493381156, 0.494692030051736, 0.70374963345836, 0.860064603995555,
                                   0.947696873553145, 0.984701714236331))
})
