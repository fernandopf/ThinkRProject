
#' day_hour
#'
#' Function to get the information from the cryptocompare API
#'
#' This function has been designed to get a dataframe from the crytocompare API with the highest, lowest, open and close price from the crytocurrency chosen in hour or day timeframe.
#' @param timeframe timeframe in hour or day
#' @param firstDay first day to analyse in dd/mm/yyyy format
#' @param lastDay last day to analyse in dd/mm/yyyy format
#' @param crytocurrenty cryptocurrency to analyse
#' @param comparison currency to be compared
#'
#' @return dataframe with the time, highest price, lowest price, open price, close price of the chosen timeframe
#' @export day_hour
#' @importFrom dplyr mutate arrange
#' @importFrom jsonlite fromJSON
#' @return dataframe with all the information required
#' @examples
day_hour <- function(timeframe, firstDay, lastDay, crytocurrenty = "BTC", comparison = "USD") {
  # Initialitaion of the data frame with all the desired output
  df <- data.frame(
    date=as.Date(character()),
    high=double(),
    low = double(),
    open = double(),
    close = double(),
    volume = double()
  )

  # Date
  firstDay <- as.Date(firstDay,format="%d/%m/%Y")
  lastDay <- as.Date(lastDay,format="%d/%m/%Y")

  time <- round(as.numeric(as.POSIXct(lastDay, format="%m/%d/%Y")))

  # Number of points of hour dataframe
  n <- as.numeric(lastDay-firstDay)

  if (timeframe %in% c("Day", "day")){
    a <- "histoday"
    incr <- 3600*24
  }
  else if (timeframe %in% c("Hour", "hour")) {
    a <- "histohour"
    n <- round(n*24)
    incr <- 3600
  }
  else {
    print('No valid timeframe')
    return();
  }

  # Maximum number of points is 2000
  if (n <= 2000) {
    link <- paste("https://min-api.cryptocompare.com/data/", a,"?fsym=",crytocurrenty, "&tsym=", comparison,"&limit=", n, "&aggregate=1&toTs=",time, "&extraParams=ThinkR", sep = "")
    linkVolume <- paste("https://min-api.cryptocompare.com/data/exchange/", a,"?tsym=",crytocurrenty,"&limit=", n, "&toTs=",time, "&extraParams=ThinkR", sep = "")
    dataPrice <- fromJSON(link)
    dataVolume <- fromJSON(linkVolume)
    df <- data.frame(
      date= as.POSIXct(dataPrice$Data$time,origin = "1970-01-01",tz = "GMT"),
      high=dataPrice$Data$high,
      low = dataPrice$Data$low,
      open = dataPrice$Data$open,
      close = dataPrice$Data$close,
      volume = dataVolume$Data$volume
    )
  }
  # If the number of points is higher than the maximum we need to do a for loop
  else {
    # Round to the highest Integuer
    iterations <- ceiling(n/2000)
    n1 <- 2000
    for (i in 1:iterations){
      if (i ==iterations){
        n1 =n-2000*(iterations-1)
      }
      linkPrice <- paste("https://min-api.cryptocompare.com/data/", a,"?fsym=",crytocurrenty, "&tsym=", comparison,"&limit=", n1, "&aggregate=1&toTs=",time, "&extraParams=ThinkR", sep = "")
      linkVolume <- paste("https://min-api.cryptocompare.com/data/exchange/", a,"?tsym=",crytocurrenty,"&limit=", n1, "&toTs=",time, "&extraParams=ThinkR", sep = "")
      dataPrice <- fromJSON(linkPrice)
      dataVolume <- fromJSON(linkVolume)
      df1 <- data.frame(
        date= as.POSIXct(dataPrice$Data$time,origin = "1970-01-01",tz = "GMT"),
        high=dataPrice$Data$high,
        low = dataPrice$Data$low,
        open = dataPrice$Data$open,
        close = dataPrice$Data$close,
        volume = dataVolume$Data$volume
      )
      df <- rbind(df, df1)
      time <- time - 2000*incr
    }
  }
  df <-df %>% mutate(direction = ifelse(open >close, "increasing", "decreasing")) %>% arrange(date)
  return(df)
}
