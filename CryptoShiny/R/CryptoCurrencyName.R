#' Returns the name of a cryptocurrency using its symbol. Uses the dataframe crp_dic.csv
#'
#' @param CryptoSymbol a character representing the cryptocurrency Symbol
#' @return Character representing the name of the wanted cryptocurrency
#' @importFrom utils read.csv2 data
#' @importFrom dplyr filter
#' @example
#' \dontrun{
#' CryptoCurrencyName("BTC")
#' }
#' @export CryptoCurrencyName

CryptoCurrencyName <- function(CryptoSymbol) {
  Symb <- crp_dic %>%
    filter(.$Symbol == CryptoSymbol)
  return(as.character(Symb$Name))
}
