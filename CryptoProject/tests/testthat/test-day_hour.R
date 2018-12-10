
firstDay <- "01/08/2017"
lastDay <- "01/01/2018"

#Number of rows
number_rows <- 24*as.numeric( as.Date(lastDay,format="%d/%m/%Y")-as.Date(firstDay,format="%d/%m/%Y"))
datasetToTest <- day_hour("hour", firstDay, lastDay, "BTC", "ETH")

# Dataset that have been checked
test1 <- read.csv("tests/testthat/data/test_day_hour1.csv", sep = ",")

 test_that("Number of point dataset:", {
   expect_equal(nrow(datasetToTest), number_rows)
 })

# Test the price of a good datased that have been checked
# test_that("Price of the dataset:", {
#   expect_equivalent(datasetToTest$high,test1$high)
# })

# Test the class of the variables of the dataset
test_that("Type of elements dataset:", {
  expect_is(datasetToTest$date, "POSIXct")
  expect_is(datasetToTest$high, "numeric")
  expect_is(datasetToTest$low, "numeric")
  expect_is(datasetToTest$open, "numeric")
  expect_is(datasetToTest$close, "numeric")
  expect_is(datasetToTest$volume, "numeric")
  expect_is(datasetToTest$direction, "character")
})

# Test the error when the initial date is higher than the actual day
higherDate <- format(Sys.time()+as.difftime(1, units="days"), "%d/%m/%Y")
test_that("Error in the function:", {
  expect_error(day_hour("hour", higherDate, lastDay, "BTC", "ETH"))
})


# Test that if the cryptocurrency to analyze and compare are the same, the high, low, open and close are a vector of ones ant the volume is a vectors of 0
ones <- rep(1, number_rows+1)
zeros <- rep(0, number_rows+1)
test_same_cryptocurrency_dataset <- day_hour("hour", firstDay, lastDay, "BTC", "BTC")

test_that("Cryptocurrency to analyze and compare are the same:", {
   expect_equivalent(test_same_cryptocurrency_dataset$high,ones)
   expect_equivalent(test_same_cryptocurrency_dataset$low,ones)
   expect_equivalent(test_same_cryptocurrency_dataset$close,ones)
   expect_equivalent(test_same_cryptocurrency_dataset$open,ones)
   expect_equivalent(test_same_cryptocurrency_dataset$volume,zeros)
 })
