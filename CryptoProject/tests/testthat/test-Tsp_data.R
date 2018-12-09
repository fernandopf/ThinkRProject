datasetToTest <- Tsp_data(now <- round(as.numeric(as.POSIXct(Sys.time(), format="%Y/%m/%d"))))

test_that("Type of elements dataset:", {
  expect_is(datasetToTest$time, "integer")
  expect_is(datasetToTest$body, "factor")
})
