#' Analyses a dataframe of news article timestamp and occurences of currencySymbols and sums the number of occurences of each currency during the hour specified in argument
#'
#' @param BoolDataNews dataframe of news article timestamp and boolean occurences of currencySymbols
#' @param CrpSymbols a vector of characters representing the cryptocurrencys Symbols to be analysed
#' @param hourTstp the timestamp of the hour to be analysed
#' @return Dataframe with a counter column per cryptosymbol, representing the number of times each cryptocurrency was cited in the news articles during the hour
#' @importFrom dplyr filter mutate
#'
SumlastHour <- function(hourTstp, CrpSymbols, BoolDataNews) {
  res <- BoolDataNews %>%
    filter(time < hourTstp, time > hourTstp - 3600)
  resu <- data.frame(t(colSums(res[-1])))
  resu <- cbind(hourTstp, resu)
  names(resu) <- c("time", CrpSymbols)
  return(resu)
}
