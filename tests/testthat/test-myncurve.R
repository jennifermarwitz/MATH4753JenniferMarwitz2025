library(testthat)

# Load the function from the package
test_that("myncurve returns correct named values", {
  result <- myncurve(mu = 5, sigma = 2, a = 6)

  # Test if the returned mu is correct
  expect_equal(result$mu, 5)

  # Test if the returned sigma is correct
  expect_equal(result$sigma, 2)

  # Test if the computed probability matches pnorm()
  expected_prob <- round(pnorm(6, mean = 5, sd = 2), 4)
  expect_equal(result$probability, expected_prob)
})
