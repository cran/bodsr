test_that("numeric value required for limit", {

  skip_on_cran()

  expect_error(
    get_timetable_metadata(limit = "25"),
    "Please provide an integer value to the limit argument")
})

test_that("helpful error message with bad authentication", {
  expect_error(
    get_timetable_metadata(api_key = "wrong key"))
})


test_that("Message giving expected number of records", {

  skip_on_cran()
  expect_message(get_timetable_metadata(limit = 5),
               "Returning 5 records")


})

test_that("test that arguments work as expected", {

  skip_on_cran()
    expect_equal(nrow(get_timetable_metadata(limit = 20)),
                 20)

    expect_equal(unique(get_timetable_metadata(status = "published")$status),
                 "published")

    expect_true(all(grepl("Stagecoach",
                          get_timetable_metadata(search = "Stagecoach")$operatorName)))


})

test_that("test that noc search succeeds and fails as expected", {

  skip_on_cran()

  expect_equal(unlist(unique(get_timetable_metadata(noc = "HUYT")$noc)),
               "HUYT")

  expect_error(get_timetable_metadata(noc = "HUY"),
               "Invalid NOC codes:HUY")


})

