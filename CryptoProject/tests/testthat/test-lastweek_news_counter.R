
test_that("Type of elements dataset:", {
  skip_if_not(curl::has_internet(), message = "no internet")
  datasetToTest <- lastweek_news_counter("BTC")
  expect_is(datasetToTest$time, "POSIXct")
  expect_is(datasetToTest$mentioned, "numeric")
})

# Test that we have an error if we dont have internet
test_that("Error in the function:", {
  skip_if(curl::has_internet(), message = "you have internet")
  expect_error(lastweek_news_counter("BTC"))
})

