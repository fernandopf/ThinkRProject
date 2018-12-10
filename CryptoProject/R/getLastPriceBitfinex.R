#' getLastPriceBitfinex
#'
#' Function to get the last price of the cryptocurrency in comparision with the desired currency of the platform Bitfinex
#'
#' @param cryptocurrency cryptocurrency id
#' @param comparison to be compared with
#'
#' @return string with the last price of Bitfinex, the criptocurrency and the comparison
#' @importFrom glue glue
#' @export getLastPriceBitfinex
#'
#' @examples
getLastPriceBitfinex <- function(cryptocurrency, comparison){
  if (cryptocurrency == comparison){
    return("1")
  }
  # Bitfinex
  urlBitfinex <- glue("https://api.bitfinex.com/v1/pubticker/{cryptocurrency}{comparison}")

  # Know if the currency exists
  res <- GET(url = urlBitfinex)
  if(res$status_code != 200){
    # If the currency is not in the platform we return 0
    return("0")
  } else {
    Bitfinex <- fromJSON(urlBitfinex)
    PriceBitfinex <-  Bitfinex$last_price
  }
  return(glue("{PriceBitfinex} {cryptocurrency}/{comparison}"))
}
