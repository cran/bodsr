test_that("returns expected message", {

  skip_on_cran()

  meta <- get_timetable_metadata(limit = 2)

  expect_message(get_timetable_data(meta[2,]))
})

test_that("data has expected size and shape", {

  skip_on_cran()

  meta <- get_timetable_metadata(limit = 5)[2:4,]

  timetable <- get_timetable_data(meta)

  expect_length(timetable, 3)
  expect_length(timetable[[1]], 12)
  expect_equal(nrow(timetable[[1]]), 96)

})
