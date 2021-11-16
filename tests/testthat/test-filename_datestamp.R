test_that("filename_datestamp", {
  expect_equal(file_datestamp("LICENSE.csv.gz"), "LICENSE." %paste% format(Sys.Date(), "%Y%m%d") %paste% ".csv.gz")
})