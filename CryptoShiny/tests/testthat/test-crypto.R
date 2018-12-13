firstDay <- "01/08/2017"
lastDay <- "01/01/2018"

test_that("Test the function crypto:", {
  skip_if_not(curl::has_internet(), message = "no internet")

  # Dataset to test
  datasetToTest <- crypto("hour", firstDay, lastDay, "BTC", "ETH",5, 26, 12, 9)
  number_rows <- 24*as.numeric( as.Date(lastDay,format="%d/%m/%Y")-as.Date(firstDay,format="%d/%m/%Y"))

  # Test number of points of the dataset
  expect_equal(nrow(datasetToTest), number_rows)

  # Dataset that have been checked
  pathTest1 <- system.file("extdata","crypto_test1.csv",package = "CryptoShiny")
  test1 <- read.csv(pathTest1, sep = ",")

  # Test the price of a good datased that have been checked
  expect_equivalent(datasetToTest$daily_average,test1$daily_average)
  expect_equivalent(datasetToTest$MA,test1$MA)
  expect_equivalent(datasetToTest$quick_EMA,test1$quick_EMA)
  expect_equivalent(datasetToTest$slow_EMA,test1$slow_EMA)
  expect_equivalent(datasetToTest$signal_MACD,test1$signal_MACD)
  expect_equivalent(datasetToTest$MACD,test1$MACD)
})
