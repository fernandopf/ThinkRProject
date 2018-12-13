
test_that("Type of elements dataset:", {
datasetToTest <- resTspToHour(analyse_crps_news(dl_data_from(as.numeric(as.POSIXct("2018-12-11 1:00:00 EST"))), c("BTC", "ETH"))
, c("BTC", "ETH"))
  expect_is(datasetToTest$time, "POSIXct")
  expect_is(datasetToTest$BTC, "numeric")
  expect_is(datasetToTest$ETH, "numeric")

})
