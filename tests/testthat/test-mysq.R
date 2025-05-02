test_that("mysq function works", {
  l <- mysq(2:4)
  expect_equal(l[1], 4)
})
