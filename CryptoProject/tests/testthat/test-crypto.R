firstDay <- "01/08/2017"
lastDay <- "01/01/2018"
datasetToTest <- crypto("hour", firstDay, lastDay, "BTC", "ETH",5, 26, 12, 9)

# Test number of points of the dataset
number_rows <- 24*as.numeric( as.Date(lastDay,format="%d/%m/%Y")-as.Date(firstDay,format="%d/%m/%Y"))
test_that("Number of point dataset:", {
  expect_equal(nrow(datasetToTest), number_rows)
})

# Dataset that have been checked
test1 <- read.csv("tests/testthat/data/crypto_test1.csv", sep = ",")

# Test the price of a good datased that have been checked
test_that("Price of the dataset:", {
  expect_equivalent(datasetToTest$daily_average,test1$daily_average)
  expect_equivalent(datasetToTest$MA,test1$MA)
  expect_equivalent(datasetToTest$quick_EMA,test1$quick_EMA)
  expect_equivalent(datasetToTest$slow_EMA,test1$slow_EMA)
  expect_equivalent(datasetToTest$signal_MACD,test1$signal_MACD)
  expect_equivalent(datasetToTest$MACD,test1$MACD)
})
