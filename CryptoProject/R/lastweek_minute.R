
#' lastweek_minute
#'
#' Function to get the information from the cryptocompare API per minute
#'
#' This function has been designed to get a dataframe from the crytocompare API with the highest, lowest, open and close price from the crytocurrency chosen in minute of the last week.
#' @param crytocurrenty cryptocurrency to analyse
#' @param comparison currency to be compared
#'
#' @return dataframe with the time, highest price, lowest price, open price, close price of the chosen timeframe
#' @export lastweek_minute
#' @importFrom dplyr mutate arrange
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#'
#' @return dataframe with all the information required
#' @examples
lastweek_minute <- function(crytocurrenty = "BTC", comparison = "USD") {

  actualTime <- round(as.numeric(Sys.time()))
  MaxLimit <- as.numeric(Sys.time()-as.difftime(7, units="days"))

  # Number of points of  dataframe
  n <- round(actualTime-MaxLimit)/60

  # Round to the highest Integuer
  iterations <- ceiling(n/2000)

  # Set the time equal to the actual
  time = actualTime
  n1 <- 2000
  for (i in 1:iterations){
    if (i ==iterations){
      n1 =n-2000*(iterations-1)
    }
    link <- glue("https://min-api.cryptocompare.com/data/histominute?fsym={crytocurrenty}&tsym={comparison}&limit={n1}&aggregate=1&toTs={time}&extraParams=your_app_name")
    data <- fromJSON(link)
    df1 <- data.frame(
      date= as.POSIXct(data$Data$time,origin = "1970-01-01",tz = "GMT"),
      high=data$Data$high,
      low = data$Data$low,
      open = data$Data$open,
      close = data$Data$close
    )
    if (i ==1){
      df <- df1
    } else {
      df <- rbind(df, df1)
    }
    time <- time - 2000*60
  }

  df <-df %>%
  mutate(direction = ifelse(open >close, "increasing", "decreasing")) %>%
  arrange(date)
  return(df)
}
