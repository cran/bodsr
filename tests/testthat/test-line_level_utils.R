test_that("faulty xml file fails cleanly", {

  xml <- "../testdata/fake_timetable.xml"

  expect_warning(line_level_xml(xml), "File could not be read in:../testdata/fake_timetable.xml")
  expect_equal(nrow(suppressWarnings(line_level_xml(xml))), 0)
  expect_equal(ncol(suppressWarnings(line_level_xml(xml))), 5)

})

test_that("example xml file returns expected data", {

  skip_on_cran()

  xml <- "../testdata/test_timetable.xml"

  expect_equal(nrow(line_level_xml(xml)), 3)
  expect_equal(ncol(line_level_xml(xml)), 5)

})

test_that("count message can be adjusted", {

  skip_on_cran()

  xml <- "../testdata/test_timetable.xml"

  expect_message(line_level_xml(xml, 2, 2), "Reading file 2 of 2")
  expect_message(line_level_xml(xml, 1, 4), "Reading file 1 of 4")

})

test_that("open_all works with line level xml function", {

  skip_on_cran()

  expect_true(tibble::is_tibble(open_all_xml("https://data.bus-data.dft.gov.uk/timetable/dataset/6/download/",
                                             line_level_xml)))
  expect_message(open_all_xml("https://data.bus-data.dft.gov.uk/timetable/dataset/6/download/",
                              line_level_xml))

})



test_that("line level data can read in xml", {

  skip_on_cran()

  meta <- bodsr::get_timetable_metadata()
  meta <- meta[meta$extension == "xml", ]

  expect_true(tibble::is_tibble(extract_line_level_data(meta[1,])))
  expect_message(extract_line_level_data(meta[1,]))

})


test_that("line level data can read in zip", {

  skip_on_cran()

  meta <- bodsr::get_timetable_metadata()
  meta <- meta[meta$extension == "zip", ]

  expect_true(tibble::is_tibble(extract_line_level_data(meta[1,])))
  expect_message(extract_line_level_data(meta[1,]))

})


test_that("line level data fails with other data types", {

  skip_on_cran()


  meta <- bodsr::get_timetable_metadata()
  meta <- meta[meta$extension == "zip", ]
  meta$extension <- "fail"

  expect_error(extract_line_level_data(meta[1,]), "Unsupported file type")

})

