

test_that("Function getLastPriceBitfinex:", {
  skip_if_not(curl::has_internet(), message = "no internet")
  priceToTest<- getLastPriceBitfinex("ETH", "BTC")
  expect_is(class(priceToTest)[2], "character")
})

# When the cryptocurrency and the comparison are the same, we return "1"

test_that("Function getLastPriceBitfinex:", {
  skip_if_not(curl::has_internet(), message = "no internet")
  priceEqualOne <- getLastPriceBitfinex("ETH", "ETH")
  expect_equivalent(priceEqualOne, "1")
})

# When the cryptocurrency is not in the platform the function return 0

test_that("Function getLastPriceBitfinex:", {
  skip_if_not(curl::has_internet(), message = "no internet")
  priceEqualZero <- getLastPriceBitfinex("ILoveCordoba", "ETH")
  expect_equivalent(priceEqualZero, "0")
})

# Test that we have an error if we dont have internet
test_that("Error in the function:", {
  skip_if(curl::has_internet(), message = "you have internet")
  expect_error(getLastPriceBitfinex("BTC", "ETH"))
})




