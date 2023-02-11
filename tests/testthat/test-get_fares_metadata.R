test_that("numeric value required for limit", {

  skip_on_cran()

  expect_error(
    get_fares_metadata(limit = "25"),
    "Please provide an integer value to the limit argument")
})

test_that("helpful error message with bad authentication", {
  expect_error(
    get_fares_metadata(api_key = "wrong_key"))

  expect_error(
    get_fares_metadata(api_key = "wrong_key"),
    "Authentication credentials are not valid; please check you are using a valid BODS API key"
    )
})


test_that("Message giving expected number of records", {

  skip_on_cran()
  expect_message(get_fares_metadata(limit = 5),
                 "Returning 5 records")


})


test_that("arguments work as expected", {

  skip_on_cran()
  expect_equal(nrow(get_fares_metadata(limit = 20)),
               20)

  expect_equal(unique(get_fares_metadata(status = "published")$status),
               "published")

  expect_equal(unlist(unique(get_fares_metadata(noc = "HUYT")$noc)),
              "HUYT")

  expect_true(all(grepl("Medway|Kent",
                        get_fares_metadata(bounding_box = c(51.451, 51.509, 0.051, 0.101))$description)))


})

test_that("noc search fails as expected", {

  skip_on_cran()


  expect_error(get_fares_metadata(noc = "HUY"),
               "Invalid NOC codes:HUY")


})

test_that("incorrect length of bounding box vector gives error", {

  skip_on_cran()


  expect_error(get_fares_metadata(bounding_box = c(1,2,3)),
               "Incorrect number of coordinates provided to bounding_box argument")


})


