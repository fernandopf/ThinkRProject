
test_that("Number of points dataset is equal to 6 (platforms analysed):", {
  skip_if_not(curl::has_internet(), message = "no internet")
  datasetToTest <- getLastPriceMultiplePlatforms("BTC")
  expect_equal(nrow(datasetToTest), 6)
})

test_that("Class of the elements of the dataset:", {
  skip_if_not(curl::has_internet(), message = "no internet")
  datasetToTest <- getLastPriceMultiplePlatforms("BTC")
  expect_is(datasetToTest$Price, "character")
  expect_is(datasetToTest$Platform, "factor")
})

# What happen when the cryptocurrency asked is not in the platforms
test_that("Price is NA:", {
  skip_if_not(curl::has_internet(), message = "no internet")
  datasetBadCurrency <- getLastPriceMultiplePlatforms("CordobaIsTheBest")
  expect_true(all(is.na(datasetBadCurrency$Price)))
})

# Test that we have an error if we dont have internet
test_that("Error in the function:", {
  skip_if(curl::has_internet(), message = "you have internet")
  expect_error(getLastPriceMultiplePlatforms("BTC"))
})



