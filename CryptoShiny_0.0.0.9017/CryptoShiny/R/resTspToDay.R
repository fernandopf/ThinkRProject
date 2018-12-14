
#' Analyses a dataframe of news article timestamp and occurences of currencySymbols and sums the number of occurences of each currency per day
#'
#' @param resTsp dataframe of news article timestamp and boolean occurences of currencySymbols
#' @param CrpSymbols a vector of characters representing the cryptocurrencys Symbols to be analysed
#' @return Dataframe with a counter column per cryptosymbol, representing the number of times each cryptocurrency was cited in the news articles diring the hour
#' @export resTspToDay
#' @importFrom dplyr filter mutate
#' @importFrom lubridate round_date

resTspToDay <- function(resTsp, CrpSymbols) {
  max <- floor_date(as.POSIXct(max(resTsp$time), origin="1970-01-01"), unit = "day") + 3600*24
  min <- floor_date(as.POSIXct(min(resTsp$time), origin="1970-01-01"), unit = "day")

  dayvec <- seq(max, min, by=-3600*24)

  resu <- data.frame(t(sapply(dayvec, FUN = SumlastDay, CrpSymbols=CrpSymbols, BoolDataNews=resTsp)))
  resu <- data.frame(sapply( resu, as.numeric ))
  resu <- resu %>%
    mutate(time = as.POSIXct(time, origin="1970-01-01"))
  return(resu)
}
