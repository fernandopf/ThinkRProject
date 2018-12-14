
#' Downloads the 20 cryptocurrencies that have the highest market capitalization from cryptocompare API
#'
#' @return vector of characters representing the symbols of the cryptocurencies
#' @export get_imp_Crp
#' @importFrom jsonlite fromJSON

get_imp_Crp <- function(){
  ndata <- fromJSON("https://min-api.cryptocompare.com/data/top/volumes?tsym=USDT")
  c(ndata$Data$SYMBOL,"USDT")
}

