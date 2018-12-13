test_that("Type of elements dataset:", {
  datasetToTest <- dl_data_from(as.numeric(as.POSIXct("2018-12-11 1:00:00 EST")))
  expect_is(datasetToTest$time, "integer")
  expect_is(datasetToTest$body, "factor")
})
