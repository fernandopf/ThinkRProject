priceToTest<- getLastPriceBitfinex("ETH", "BTC")

test_that("Class of the price:", {
  expect_is(class(priceToTest)[2], "character")
})

# When the cryptocurrency and the comparison are the same, we return "1"
priceEqualOne <- getLastPriceBitfinex("ETH", "ETH")

test_that("Class of the price:", {
  expect_equivalent(priceEqualOne, "1")
})

# When the cryptocurrency is not in the platform the function return 0
priceEqualZero <- getLastPriceBitfinex("ILoveCordoba", "ETH")

test_that("Class of the price:", {
  expect_equivalent(priceEqualZero, "0")
})

