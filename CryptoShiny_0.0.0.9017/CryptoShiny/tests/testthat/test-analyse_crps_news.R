
test_that("Type of elements dataset:", {
  datasetToTest <- analyse_crps_news(dl_data_from(as.numeric(as.POSIXct("2018-12-11 1:00:00 EST"))), c("BTC", "ETH"))
  expect_is(datasetToTest$time, "numeric")
  expect_is(datasetToTest$BTC, "numeric")
  expect_is(datasetToTest$ETH, "numeric")

})
