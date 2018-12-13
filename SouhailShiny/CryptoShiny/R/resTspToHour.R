#' Analyses a dataframe of news article timestamp and occurences of currencySymbols and sums the number of occurences of each currency per hour
#'
#' @param resTsp dataframe of news article timestamp and boolean occurences of currencySymbols
#' @param CrpSymbols a vector of characters representing the cryptocurrencys Symbols to be analysed
#' @return Dataframe with a counter column per cryptosymbol, representing the number of times each cryptocurrency was cited in the news articles diring the hour
#' @export resTspToHour
#' @importFrom dplyr filter mutate
#' @importFrom lubridate round_date

resTspToHour <- function(resTsp, CrpSymbols) {
  max <- floor_date(as.POSIXct(max(resTsp$time), origin="1970-01-01"), unit = "hour") + 3600
  min <- floor_date(as.POSIXct(min(resTsp$time), origin="1970-01-01"), unit = "hour")

  hourvec <- seq(max, min, by=-3600)
  resul <- NULL
  for (hour in hourvec) {
    resul <- rbind(resul, SumlastHour(hour, CrpSymbols, resTsp))
  }
  return(resul %>%
           mutate(time = as.POSIXct(time, origin="1970-01-01")))
}
