#' Analyses a dataframe with news body for occurences of the each of the currencySymbols
#'
#' @param databodynews dataframe with news body and their timestamp
#' @param currencySymbols a vector of characters representing the cryptocurrencys Symbols to be analysed
#' @return Dataframe with a boolean column per cryptosymbol, representing if the cryptocurrency was cited in the news article
#' @export analyse_crps_news
#' @importFrom dplyr filter mutate
#'
analyse_crps_news <- function(databodynews, currencySymbols) {
  dataNews <- data.frame(sapply(currencySymbols, FUN = analyse_crp_news, databodynews=databodynews))
  names(dataNews) <- currencySymbols
  dataNews <- cbind(databodynews[1], dataNews)
  dataNews$time <- as.numeric(dataNews$time)
  return(dataNews)
}
