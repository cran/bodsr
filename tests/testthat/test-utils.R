test_that("not_null pastes together objects if not null", {

  x = 2
  expect_equal(not_null(x, "number"), "number=2&")

  })

test_that("not_null returns null correctly", {

  x = NULL
  expect_null(not_null(x, "number"))

  })

test_that("not_null_date rejects non-date arguments",{

  x = "2022-01-01"
  expect_error(not_null_date(x, "start_date"))

  x = as.Date(x)
  expect_equal(not_null_date(x, "start_date"),
               "start_date=2022-01-01T00%3A00%3A00&")

  })

test_that("not_null returns null correctly", {

  x = NULL
  expect_null(not_null_date(x, "number"))

})


test_that("failed namespace search returns null", {

  xml_test <- xml2::read_xml("../testdata/note.xml")

  expect_null(find_node_value(xml_test, "a"))
  expect_equal(count_nodes(xml_test, "a"), 0)

})

test_that("expected nodes are found in example xml file", {

  xml_test <- xml2::read_xml("../testdata/test_timetable.xml")

  expect_equal(find_node_value(xml_test, "//d1:TradingName"), "Lynx")
  expect_equal(count_nodes(xml_test, "//d1:TradingName"), 1)

})


test_that("reading in xml file fails and succeeds as expected", {

  expect_length(poss_xml("../testdata/test_timetable.xml"), 2)
  expect_null(poss_xml("../testdata/test_timetabl.xml"))

})
