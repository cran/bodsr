test_that("check that authentication fail returns error", {

  skip_on_cran()

  expect_error(get_location_gtfs(api_key = "wrong_key"))
})


test_that("check that bad request returns error", {

  skip_on_cran()

  expect_error(get_location_gtfs(route_id = "abc", start_time_after = "??"))
})

test_that("check that wrong bounding box returns error", {

  skip_on_cran()

  expect_error(get_location_gtfs(bounding_box = c(4, 5, 2)))
})

test_that("filters work as expected", {

  skip_on_cran()

  expect_equal(length(get_location_gtfs()), 10)
})

