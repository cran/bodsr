test_that("function returns a table of two cols", {

  skip_on_cran()

  expect_true(is.data.frame(noc_lookup()))
  expect_equal(ncol(noc_lookup()), 2)
})
