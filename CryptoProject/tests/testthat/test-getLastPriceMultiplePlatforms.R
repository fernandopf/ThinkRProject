datasetToTest <- getLastPriceMultiplePlatforms("BTC")

test_that("Number of points dataset is equal to 6 (platforms analysed):", {
  expect_equal(nrow(datasetToTest), 6)
})

test_that("Class of the elements of the dataset:", {
  expect_is(datasetToTest$Price, "character")
  expect_is(datasetToTest$Platform, "factor")
})

# What happen when the cryptocurrency asked is not in the platforms
datasetBadCurrency <- getLastPriceMultiplePlatforms("CordobaIsTheBest")
test_that("Price is NA:", {
  expect_true(all(is.na(datasetBadCurrency$Price)))
})


