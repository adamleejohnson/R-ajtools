test_that("filename_datestamp", {
  expect_equal(file_datestamp("LICENSE.csv.gz"), "LICENSE." %++% format(Sys.Date(), "%Y%m%d") %++% ".csv.gz")
})
