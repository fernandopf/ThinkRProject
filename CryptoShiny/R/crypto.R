#' Crypto
#'
#' Master function
#'
#' Master function which aggragate all the function of our package
#'
#' @param timeframe timeframe in hour or day
#' @param firstDay first day to analyse in dd/mm/yyyy format
#' @param lastDay last day to analyse in dd/mm/yyyy format
#' @param cryptocurrency cryptocurrency to analyse
#' @param comparison currency to be compared
#' @param n_MA Window Moving Average
#' @param n_quick_MACD quick MACD
#' @param n_slow_MACD slow MACD
#' @param n_signal_MACD signal MACD
#'
#' @return dataframe with the time, highest price, lowest price, open price, close price and financial indicators in the chosen timeframe
#' @importFrom readr read_csv
#' @importFrom dplyr left_join
#'
#' @examples
#' \dontrun{
#' crypto("hour", "01/08/2018", "01/10/2018", "BTC", "USD",5 , 26, 12, 9)
#' }
#' @export

crypto <-
  function(timeframe,
           firstDay,
           lastDay,
           cryptocurrency = "BTC",
           comparison = "USD",
           n_MA,
           n_quick_MACD,
           n_slow_MACD,
           n_signal_MACD) {
    if (timeframe %in% c("Week", "week", "Month", "month")) {
      df <-
        day_hour(timeframe = "day", firstDay, lastDay, cryptocurrency, comparison)

      CryptoNewsAnalysed <-
        CryptoNewsOccurencesDays[c("time", cryptocurrency)]

      colnames(CryptoNewsAnalysed) <- c("date", "news")

      attr(CryptoNewsAnalysed$date, "tzone") <- "GMT"
      CryptoNewsAnalysed$date <- CryptoNewsAnalysed$date + 3600

      df.news <-
        df %>% left_join(CryptoNewsAnalysed, by = "date")

      df.transformed <-
        weekly_monthly_transformation(df.news, timeframe)

      df.averaged <-
        averages(df.transformed,
                 n_MA,
                 n_quick_MACD,
                 n_slow_MACD,
                 n_signal_MACD)
    }
    else {
      df <-
        day_hour(timeframe, firstDay, lastDay, cryptocurrency, comparison)

      if (timeframe %in% c("Day", "day")) {
        CryptoNewsAnalysed <-
          CryptoNewsOccurencesDays }
      else {CryptoNewsAnalysed <-
        CryptoNewsOccurencesHour}

        CryptoNewsAnalysed <-
          CryptoNewsAnalysed[c("time", cryptocurrency)]

        colnames(CryptoNewsAnalysed) <- c("date", "news")
        attr(CryptoNewsAnalysed$date, "tzone") <- "GMT"
        CryptoNewsAnalysed$date <- CryptoNewsAnalysed$date + 3600

        df.news <-
          df %>% left_join(CryptoNewsAnalysed, by = "date")

        df.averaged <-
          averages(df.news, n_MA, n_quick_MACD, n_slow_MACD, n_signal_MACD)

      }
      return(df.averaged)
    }
