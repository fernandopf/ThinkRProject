
#' Analyses a dataframe of news article timestamp and occurences of currencySymbols and sums the number of occurences of each currency during the day specified in argument
#'
#' @param dayTstp the timestamp of the hour to be analysed
#' @param CrpSymbols a vector of characters representing the cryptocurrencys Symbols to be analysed
#' @param BoolDataNews dataframe of news article timestamp and boolean occurences of currencySymbols
#' @return Dataframe with a counter column per cryptosymbol, representing the number of times each cryptocurrency was cited in the news articles during the hour
#' @export SumlastDay
#' @importFrom dplyr filter mutate

SumlastDay <- function(dayTstp, CrpSymbols, BoolDataNews) {
  res <- BoolDataNews %>%
    filter(.$time<dayTstp, .$time>dayTstp-3600*24)
  resu <- dayTstp
  for (i in 2:length(res)) {
    resa <- res %>%
      filter(res[i] == 1)
    resu <- cbind(resu, length(resa$time))
  }
  resu <- data.frame(resu)
  names(resu) <- c("time", CrpSymbols)
  return(resu)
}
