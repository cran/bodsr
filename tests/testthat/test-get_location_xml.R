test_that("returns error with wrong API key", {

  expect_error(
    get_location_xml(api_key = "wrong key"))

})

test_that("returns error with incorrect bounding box", {

  skip_on_cran()

  expect_error(
    get_location_xml(bounding_box = c(2, 4, 5)))


})


test_that("returns error with wrong NOC code", {

  skip_on_cran()

  expect_error(get_location_xml(noc = "HUY"),
               "Invalid NOC codes:HUY")


})


test_that("filters return expected values", {

  skip_on_cran()

  expect_true(is.list(get_location_xml()))
  expect_length(get_location_xml(), 2)

  expect_true(is.list(get_location_xml(noc = "HUYT")))
  expect_length(get_location_xml(noc = "HUYT"), 2)


})

