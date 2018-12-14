firstDay <- "01/08/2017"
lastDay <- "01/02/2018"


test_that("Number of point dataset:", {
  skip_if_not(curl::has_internet(), message = "no internet")

  # Get the dataset from the function day_hour
  df <- day_hour("hour", firstDay, lastDay, "BTC", "ETH")
  CryptoNewsAnalysed <-CryptoNewsOccurencesDays[c("time", "BTC")]
  colnames(CryptoNewsAnalysed) <- c("date", "news")
  attr(CryptoNewsAnalysed$date, "tzone") <- "GMT"
  CryptoNewsAnalysed$date <- CryptoNewsAnalysed$date + 3600
  datasetToTransform <-df %>% dplyr::left_join(CryptoNewsAnalysed, by = "date")



  datasetTotestMonth <- weekly_monthly_transformation(datasetToTransform, "Month" )
  datasetTotestWeek <- weekly_monthly_transformation(datasetToTransform, "Week" )

  firstDayDate <- as.Date(firstDay,format="%d/%m/%Y")
  lastDayDate <-  as.Date(lastDay,format="%d/%m/%Y")

  # Number of months and weeks between first and last day, that will be the number of observations
  NumberOfpointsMonth <-  length(seq(from=firstDayDate, to=lastDayDate, by='month'))
  NumberOfpointsWeek <-  length(seq(from=firstDayDate, to=lastDayDate, by='week'))

  # Tests
  expect_equal(nrow(datasetTotestMonth), NumberOfpointsMonth)
  expect_equal(nrow(datasetTotestWeek), NumberOfpointsWeek)
})

# Test the class of the variables of the dataset
test_that("Type of elements dataset:", {
  skip_if_not(curl::has_internet(), message = "no internet")

  # Get the dataset from the function day_hour

  df <- day_hour("hour", firstDay, lastDay, "BTC", "ETH")
  CryptoNewsAnalysed <-CryptoNewsOccurencesDays[c("time", "BTC")]
  colnames(CryptoNewsAnalysed) <- c("date", "news")
  attr(CryptoNewsAnalysed$date, "tzone") <- "GMT"
  CryptoNewsAnalysed$date <- CryptoNewsAnalysed$date + 3600
  datasetToTransform <-df %>% dplyr::left_join(CryptoNewsAnalysed, by = "date")

  datasetTotestMonth <- weekly_monthly_transformation(datasetToTransform, "Month" )

  expect_is(datasetTotestMonth$date, "POSIXct")
  expect_is(datasetTotestMonth$high, "numeric")
  expect_is(datasetTotestMonth$low, "numeric")
  expect_is(datasetTotestMonth$open, "numeric")
  expect_is(datasetTotestMonth$close, "numeric")
  expect_is(datasetTotestMonth$volume, "numeric")
  expect_is(datasetTotestMonth$direction, "character")
})
