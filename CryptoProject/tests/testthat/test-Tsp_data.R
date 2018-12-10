

test_that("Type of elements dataset:", {
  skip_if_not(curl::has_internet(), message = "no internet")
  datasetToTest <- Tsp_data(now <- round(as.numeric(as.POSIXct(Sys.time(), format="%Y/%m/%d"))))
  expect_is(datasetToTest$time, "integer")
  expect_is(datasetToTest$body, "factor")
})
