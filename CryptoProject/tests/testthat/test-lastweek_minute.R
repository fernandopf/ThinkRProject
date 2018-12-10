
# Number of rows of the dataset
actualTime <- round(as.numeric(Sys.time()))
MaxLimit <- as.numeric(Sys.time()-as.difftime(7, units="days"))
number_rows <- round(actualTime-MaxLimit)/60


test_that("Number of point dataset:", {

  skip_if_not(curl::has_internet(), message = "no internet")
  datasetToTest <- lastweek_minute( "BTC", "ETH")

  #Number of rows
  actualTime <- round(as.numeric(Sys.time()))
  MaxLimit <- as.numeric(Sys.time()-as.difftime(7, units="days"))
  number_rows <- round(actualTime-MaxLimit)/60
  expect_equal(nrow(datasetToTest), number_rows)

  # Test the class of the variables of the dataset
  expect_is(datasetToTest$date, "POSIXct")
  expect_is(datasetToTest$high, "numeric")
  expect_is(datasetToTest$low, "numeric")
  expect_is(datasetToTest$open, "numeric")
  expect_is(datasetToTest$close, "numeric")
  expect_is(datasetToTest$direction, "character")
})



# Test that if the cryptocurrency to analyze and compare are the same, the high, low, open and close are a vector of ones

test_that("Cryptocurrency to analyze and compare are the same:", {
  skip_if_not(curl::has_internet(), message = "no internet")
  ones <- rep(1, number_rows)
  zeros <- rep(0, number_rows)
  test_same_cryptocurrency_dataset <- lastweek_minute("BTC", "BTC")
  expect_equivalent(test_same_cryptocurrency_dataset$high,ones)
  expect_equivalent(test_same_cryptocurrency_dataset$low,ones)
  expect_equivalent(test_same_cryptocurrency_dataset$close,ones)
  expect_equivalent(test_same_cryptocurrency_dataset$open,ones)
})


# Test that we have an error if we dont have internet
test_that("Error in the function:", {
  skip_if(curl::has_internet(), message = "you have internet")
  expect_error(lastweek_minute("BTC", "BTC"))
})

