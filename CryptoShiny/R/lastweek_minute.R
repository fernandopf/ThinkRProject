

#' lastweek_minute
#'
#' Function to get the information from the cryptocompare API per minute
#'
#' This function has been designed to get a dataframe from the cryptocompare API with the highest, lowest, open and close price from the cryptocurrency chosen in minute of the last week.
#' @param cryptocurrency cryptocurrency to analyse
#' @param comparison currency to be compared
#' @param only_two_minutes whether only output last two minutes data
#'
#' @return dataframe with the time, highest price, lowest price, open price, close price of the chosen timeframe
#' @importFrom dplyr mutate arrange
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @importFrom curl has_internet
#'
#' @return dataframe with all the information required
#' @examples
#' \dontrun{
#' lastweek_minute("BTC", "USD")
#' }
#' @export

lastweek_minute <-
  function(cryptocurrency = "BTC",
           comparison = "USD",
           only_two_minutes = FALSE) {
    # First we need to check that we have internet connection (needed to call the API)
    if (!has_internet()) {
      stop("You don't have internet connection")
    }

    #  If only_two_minutes is TRUE, returns only last two minutes price
    if (only_two_minutes) {
      actualTime <- round(as.numeric(Sys.time()))
      MaxLimit <- as.numeric(Sys.time() - as.difftime(2, units = "mins"))
    }
    else {
      actualTime <- round(as.numeric(Sys.time()))
      MaxLimit <- as.numeric(Sys.time() - as.difftime(7, units = "days"))
    }

    # Number of points of  dataframe
    n <- round(actualTime - MaxLimit) / 60
    # If the cryptocurrency and the comparsion are the same, we return one dataset with all the prices equal to 1 and the volume equal to 0.

    if (cryptocurrency == comparison) {
      dates <- sapply(0:(n - 1), function(i) {
        MaxLimit + i * 60
      })
      df <- data.frame(
        date = as.POSIXct(dates, origin = "1970-01-01", tz = "GMT"),
        high = 1,
        low = 1,
        open = 1,
        close = 1
      )
    } else {
      # Round to the highest Integuer
      iterations <- ceiling(n / 2000)

      # Set the time equal to the actual
      time = actualTime
      n1 <- 2000 - 1
      for (i in 1:iterations) {
        if (i == iterations) {
          n1 = n - 2000 * (iterations - 1) - 1
        }
        link <-
          glue(
            "https://min-api.cryptocompare.com/data/histominute?fsym={cryptocurrency}&tsym={comparison}&limit={n1}&aggregate=1&toTs={time}&extraParams=your_app_name"
          )
        data <- fromJSON(link)
        df1 <- data.frame(
          date = as.POSIXct(data$Data$time, origin = "1970-01-01", tz = "GMT"),
          high = data$Data$high,
          low = data$Data$low,
          open = data$Data$open,
          close = data$Data$close
        )
        if (i == 1) {
          df <- df1
        } else {
          df <- rbind(df, df1)
        }
        time <- time - 2000 * 60
      }
    }
    df <- df %>%
      mutate(direction = ifelse(open > close, "increasing", "decreasing")) %>%
      arrange(date)
    return(df)
  }
