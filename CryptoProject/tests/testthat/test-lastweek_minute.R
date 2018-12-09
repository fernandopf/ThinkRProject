datasetToTest <- lastweek_minute( "BTC", "ETH")

#Number of rows
actualTime <- round(as.numeric(Sys.time()))
MaxLimit <- as.numeric(Sys.time()-as.difftime(7, units="days"))
number_rows <- round(actualTime-MaxLimit)/60

test_that("Number of point dataset:", {
  expect_equal(nrow(datasetToTest), number_rows)
})


# Test the class of the variables of the dataset
test_that("Number of point dataset:", {
  expect_is(datasetToTest$date, "POSIXct")
  expect_is(datasetToTest$high, "numeric")
  expect_is(datasetToTest$low, "numeric")
  expect_is(datasetToTest$open, "numeric")
  expect_is(datasetToTest$close, "numeric")
  expect_is(datasetToTest$direction, "character")
})

# Test that if the cryptocurrency to analyze and compare are the same, the high, low, open and close are a vector of ones
ones <- rep(1, number_rows)
zeros <- rep(0, number_rows)
test_same_cryptocurrency_dataset <- lastweek_minute("BTC", "BTC")

test_that("Cryptocurrency to analyze and compare are the same:", {
  expect_equivalent(test_same_cryptocurrency_dataset$high,ones)
  expect_equivalent(test_same_cryptocurrency_dataset$low,ones)
  expect_equivalent(test_same_cryptocurrency_dataset$close,ones)
  expect_equivalent(test_same_cryptocurrency_dataset$open,ones)
})
