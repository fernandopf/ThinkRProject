

test_that("Crypto correlation between two cryptocurrencies is:", {
  expect_equal(round(crypto_correlation("01/01/2017", "01/01/2018", "BTC", "ETH"),2), 0.89)
})


