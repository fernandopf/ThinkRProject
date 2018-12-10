
test_that("Crypto correlation between two cryptocurrencies is:", {
  skip_if_not(curl::has_internet(), message = "no internet")
  expect_equal(round(crypto_correlation("01/01/2017", "01/01/2018", "BTC", "ETH"),2), 0.89)
})


