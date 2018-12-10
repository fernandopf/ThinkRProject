
datasetToTest <- lastweek_news_counter("BTC")

test_that("Type of elements dataset:", {
  skip_if_not(curl::has_internet(), message = "no internet")
  expect_is(datasetToTest$time, "POSIXct")
  expect_is(datasetToTest$mentioned, "numeric")
})
