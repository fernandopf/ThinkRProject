datasetToTest <- lastweek_news_counter("BTC")

test_that("Type of elements dataset:", {
  expect_is(datasetToTest$time, "POSIXct")
  expect_is(datasetToTest$mentioned, "numeric")
})
