firstDay <- "01/08/2017"
lastDay <- "01/01/2018"

test_that("Function day_hour:", {
  skip_if_not(curl::has_internet(), message = "no internet")

  # Set the dates to have more than one call to the API

  datasetToTest <- day_hour("hour", firstDay, lastDay, "BTC", "ETH")

  #Number of rows
  number_rows <- 24*as.numeric( as.Date(lastDay,format="%d/%m/%Y")-as.Date(firstDay,format="%d/%m/%Y"))
  expect_equal(nrow(datasetToTest), number_rows)

  # Dataset that have been checked
  pathTest1 <- system.file("extdata","day_hour_test1.csv",package = "CryptoShiny")
  test1 <- read.csv(pathTest1, sep = ",")

   # Test the price of a good datased that have been checked
  expect_equivalent(datasetToTest$high,test1$high)

  # Test the class of the variables of the dataset
  expect_is(datasetToTest$date, "POSIXct")
  expect_is(datasetToTest$high, "numeric")
  expect_is(datasetToTest$low, "numeric")
  expect_is(datasetToTest$open, "numeric")
  expect_is(datasetToTest$close, "numeric")
  expect_is(datasetToTest$volume, "numeric")
  expect_is(datasetToTest$direction, "character")


})

# Test the error when the initial or last date is higher than the actual day
test_that("Error in the function:", {
  skip_if_not(curl::has_internet(), message = "no internet")
  higherDate <- format(Sys.time()+as.difftime(1, units="days"), "%d/%m/%Y")
  expect_error(day_hour("hour", higherDate, lastDay, "BTC", "ETH"))
})

# Test the error when the initial date is higher than lastday
test_that("Error in the function:", {
  skip_if_not(curl::has_internet(), message = "no internet")
  expect_error(day_hour("hour", "02/01/2018", "01/01/2018", "BTC", "ETH"))
})

# Test that if the cryptocurrency to analyze and compare are the same, the high, low, open and close are a vector of ones ant the volume is a vectors of 0
test_that("Cryptocurrency to analyze and compare are the same:", {
  skip_if_not(curl::has_internet(), message = "no internet")
  number_rows <- 24*as.numeric( as.Date(lastDay,format="%d/%m/%Y")-as.Date(firstDay,format="%d/%m/%Y"))
  ones <- rep(1, number_rows+1)
  zeros <- rep(0, number_rows+1)
  test_same_cryptocurrency_dataset <- day_hour("hour", firstDay, lastDay, "BTC", "BTC")
  expect_equivalent(test_same_cryptocurrency_dataset$high,ones)
  expect_equivalent(test_same_cryptocurrency_dataset$low,ones)
  expect_equivalent(test_same_cryptocurrency_dataset$close,ones)
  expect_equivalent(test_same_cryptocurrency_dataset$open,ones)
  expect_equivalent(test_same_cryptocurrency_dataset$volume,zeros)
})

# Test that we have an error if we dont have internet
test_that("Error in the function:", {
  skip_if(curl::has_internet(), message = "you have internet")
  expect_error(day_hour("hour", "02/01/2018", "10/01/2018", "BTC", "ETH"))
})
