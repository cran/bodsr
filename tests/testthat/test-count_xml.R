test_that("counts individual xml files", {

  skip_on_cran()

  meta <- get_timetable_metadata(limit = 10)
  meta <- meta[meta$extension == "xml",]

  expect_equal(xml_file_counter(meta), rep(1, length(meta$id)))
})


test_that("counts inside zip files", {
  skip_on_cran()

  meta <- get_timetable_metadata(limit = 10)
  meta <- meta[meta$extension == "zip",]

  expect_equal(length(xml_file_counter(meta)), length(meta$id))
  expect_true(all(xml_file_counter(meta) >= 1))
})
