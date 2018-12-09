firstDay <- "01/08/2017"
lastDay <- "01/01/2018"
datasetToTest <- crypto("hour", firstDay, lastDay, "BTC", "ETH",5, 26, 12, 9)

# Test number of points of the dataset
number_rows <- 24*as.numeric( as.Date(lastDay,format="%d/%m/%Y")-as.Date(firstDay,format="%d/%m/%Y"))
test_that("Number of point dataset:", {
  expect_equal(nrow(datasetToTest), number_rows)
})

