firstDay <- "01/08/2017"
lastDay <- "01/01/2018"

datasetToDoAverages <- day_hour("hour", firstDay, lastDay, "BTC", "ETH")
datasetToTest <- averages(datasetToDoAverages, 5, 26, 12, 9)

# Test number of points of the dataset
number_rows <- 24*as.numeric( as.Date(lastDay,format="%d/%m/%Y")-as.Date(firstDay,format="%d/%m/%Y"))
test_that("Number of point dataset:", {
  expect_equal(nrow(datasetToTest), number_rows)
})

# Test the first value and fith value of the moving average
# The first, as we dont have enough point yet is the average between the high and low value
firstMA <- (datasetToDoAverages$high[1]+datasetToDoAverages$low[1])/2
fithMA <- sum(datasetToDoAverages$high[1:5]+datasetToDoAverages$low[1:5])/(2*5)
test_that("Number of point dataset:", {
  expect_equal(datasetToTest$MA[1], firstMA)
  expect_equal(datasetToTest$MA[5], fithMA)
})

# Test the value of the daily average
test_that("Number of point dataset:", {
  expect_equal(datasetToTest$daily_average, (datasetToDoAverages$high+datasetToDoAverages$low)/2)
})

test_that("Type of elements dataset:", {
  expect_is(datasetToTest$date, "POSIXct")
  expect_is(datasetToTest$high, "numeric")
  expect_is(datasetToTest$low, "numeric")
  expect_is(datasetToTest$open, "numeric")
  expect_is(datasetToTest$close, "numeric")
  expect_is(datasetToTest$volume, "numeric")
  expect_is(datasetToTest$direction, "character")
  expect_is(datasetToTest$daily_average, "numeric")
  expect_is(datasetToTest$MA, "numeric")
  expect_is(datasetToTest$quick_EMA, "numeric")
  expect_is(datasetToTest$slow_EMA, "numeric")
  expect_is(datasetToTest$signal_MACD, "numeric")
  expect_is(datasetToTest$MACD, "numeric")
})

# Check the dataset with less point that the minimum needed to do the MV return an error

firstDay <- "01/08/2018"
lastDay <- "04/08/2018"
datasetToDoAverages <- day_hour("day", firstDay, lastDay, "BTC", "ETH")

test_that("Number of point dataset:", {
  expect_error( averages(datasetToDoAverages, 5, 5, 5, 5))
})
