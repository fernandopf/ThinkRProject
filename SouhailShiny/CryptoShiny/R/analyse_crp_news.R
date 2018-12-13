#' Analyses a dataframe with news body for occurences of the currencySymbol
#'
#' @param databodynews dataframe with news body and their timestamp
#' @param currencySymbol a character representing the cryptocurrency Symbol
#' @return Dataframe with a boolean column representing if the cryptocurrency was cited in the news article
#' @export analyse_crp_news
#' @importFrom dplyr filter mutate
#' @importFrom data.table :=
#'
analyse_crp_news <- function(databodynews, currencySymbol) {
  currencyName <- CryptoCurrencyName(currencySymbol)
  analysed <- databodynews %>%
    mutate(!!currencySymbol :=  ifelse(grepl(as.character(currencyName),body,ignore.case = TRUE), 1, 0) )
  return(analysed)
}
