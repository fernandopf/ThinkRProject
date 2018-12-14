#' day_hour
#'
#' Function to get the information from the cryptocompare API
#'
#' This function has been designed to get a dataframe from the cryptocurrency API with the highest, lowest, open and close price from the cryptocurrency chosen in hour or day timeframe.
#' @param timeframe timeframe in hour or day
#' @param firstDay first day to analyse in dd/mm/yyyy format : if the input is the same as the lastDay, will be set as the day before lastDay
#' @param lastDay last day to analyse in dd/mm/yyyy format
#' @param cryptocurrency cryptocurrency to analyse
#' @param comparison currency to be compared
#'
#' @return dataframe with the time, highest price, lowest price, open price, close price of the chosen timeframe
#' @importFrom dplyr mutate arrange
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @importFrom curl has_internet

#' @return dataframe with all the information required
#' @examples
#' \dontrun{
#' exampleSameCryptocurrency <- day_hour("day", "01/12/2017", "01/08/2018", "BTC", "BTC")
#' }
#' @export
day_hour <-
  function(timeframe,
           firstDay,
           lastDay,
           cryptocurrency = "BTC",
           comparison = "USD") {
    # First we need to check that we have internet connection (needed to call the API)
    if (!has_internet()) {
      stop("You don't have internet connection")
    }

    # Date
    firstDay <- as.Date(firstDay, format = "%d/%m/%Y")
    lastDay <- as.Date(lastDay, format = "%d/%m/%Y")

    # Error when firstDay is higher than lastDay
    if (firstDay > lastDay) {
      stop("FirstDay cannot be higher than LastDay")
    }

    # Error when lastDay is higher than actual date
    if (lastDay > Sys.time()) {
      stop("LastDay cannot be higher than actualDate")
    }

    #if the start day and the end day in same day: set start day = end day - 1
    if (firstDay == lastDay) {
      firstDay <- firstDay - 1
    }

    time <- round(as.numeric(as.POSIXct(lastDay, format = "%m/%d/%Y")))

    # Number of points of hour dataframe
    n <- as.numeric(lastDay - firstDay)

    if (timeframe %in% c("Day", "day")) {
      a <- "histoday"
      incr <- 3600 * 24
    }
    else if (timeframe %in% c("Hour", "hour")) {
      a <- "histohour"
      n <- round(n * 24)
      incr <- 3600
    }
    else {
      print('No valid timeframe')
      return()

    }
    # If the cryptocurrency and the comparsion are the same, we return one dataset with all the prices equal to 1 and the volume equal to 0.


    if (cryptocurrency == comparison) {
      dates <- sapply(0:n, function(i) {
        as.numeric(as.POSIXct(firstDay, format = "%m/%d/%Y")) + (i) * incr
      })
      df <- data.frame(
        date = as.POSIXct(dates, origin = "1970-01-01", tz = "GMT"),
        high = 1,
        low = 1,
        open = 1,
        close = 1,
        volume = 0
      )
    } else {
      # Maximum number of points is 2000
      if (n <= 2000) {
        link <-
          glue(
            "https://min-api.cryptocompare.com/data/{a}?fsym={cryptocurrency}&tsym={comparison}&limit={n}&aggregate=1&toTs={time}&extraParams=ThinkR"
          )
        linkVolume <-
          glue(
            "https://min-api.cryptocompare.com/data/exchange/{a}?tsym={cryptocurrency}&limit={n}&toTs={time}&extraParams=ThinkR"
          )
        dataPrice <- fromJSON(link)
        dataVolume <- fromJSON(linkVolume)
        df <- data.frame(
          date = as.POSIXct(dataPrice$Data$time, origin = "1970-01-01", tz = "GMT"),
          high = dataPrice$Data$high,
          low = dataPrice$Data$low,
          open = dataPrice$Data$open,
          close = dataPrice$Data$close,
          volume = dataVolume$Data$volume
        )
      }
      # If the number of points is higher than the maximum we need to do a for loop
      else {
        # Round to the highest Integuer
        iterations <- ceiling(n / 2000)
        n1 <- 2000 - 1
        # For to get data from the API several times
        for (i in 1:iterations) {
          if (i == iterations) {
            n1 = n - 2000 * (iterations - 1) - 1
          }
          linkPrice <-
            glue(
              "https://min-api.cryptocompare.com/data/{a}?fsym={cryptocurrency}&tsym={comparison}&limit={n1}&aggregate=1&toTs={time}&extraParams=ThinkR"
            )
          linkVolume <-
            glue(
              "https://min-api.cryptocompare.com/data/exchange/{a}?tsym={cryptocurrency}&limit={n1}&toTs={time}&extraParams=ThinkR"
            )
          dataPrice <- fromJSON(linkPrice)
          dataVolume <- fromJSON(linkVolume)
          df1 <- data.frame(
            date = as.POSIXct(
              dataPrice$Data$time,
              origin = "1970-01-01",
              tz = "GMT"
            ),
            high = dataPrice$Data$high,
            low = dataPrice$Data$low,
            open = dataPrice$Data$open,
            close = dataPrice$Data$close,
            volume = dataVolume$Data$volume
          )
          if (i == 1) {
            df <- df1
          } else {
            df <- rbind(df, df1)
          }
          time <- time - 2000 * incr
        }
      }
    }
    df <- df %>%
      mutate(direction = ifelse(open > close, "increasing", "decreasing")) %>%
      arrange(date)
    return(df)
  }
