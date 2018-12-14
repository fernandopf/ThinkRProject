#' getLastPriceMultiplePlatforms
#'
#' Function used to get the last price of the imputted crypto on the 6 biggest platforms
#'
#' @param cryptocurrency cryptocurrency id
#'
#' @return dataframe with prices on six platforms
#' @importFrom dplyr filter select desc
#' @importFrom glue glue
#' @importFrom lubridate floor_date
#' @importFrom rlang is_empty
#' @importFrom utils head tail
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom curl has_internet
#' @export
#'
#' @examples
#' \dontrun{
#' getLastPriceMultiplePlatforms("BTC")
#' }
getLastPriceMultiplePlatforms <- function(cryptocurrency){

  # First we need to check that we have internet connection (needed to call the API)
  if (!has_internet()){
    stop("You don't have internet connection")
  }

  # Bittrex
  linkBittrex <- "https://bittrex.com/api/v1.1/public/getmarketsummaries"
  dataBittrex <- fromJSON(linkBittrex)
  bittrex <- data.frame(crypto = dataBittrex$result$MarketName, last = dataBittrex$result$Last) %>%
    filter (crypto == glue("USDT-{cryptocurrency}")) %>%
    select("last")
  df <- data.frame(
      Platform = "Bittrex",
      Price = round(as.numeric(bittrex), digits = 2)
  )

  # Bitfinex
  urlBitfinex <- glue("https://api.bitfinex.com/v1/pubticker/{cryptocurrency}usd")
  # Know if the currency exists
  res <- GET(url = urlBitfinex)
  if(res$status_code != 200){
    PriceBitfinex <- NA
  } else {
    Bitfinex <- fromJSON(urlBitfinex)
    PriceBitfinex <-  Bitfinex$last_price
  }
  df1 <- data.frame(
    Platform = "Bitfinex",
    Price = PriceBitfinex
  )
  df <- rbind(df, df1)

  # Poliniex
  urlPoliniex <-  "https://poloniex.com/public?command=returnTicker"
  Poliniex <- fromJSON(urlPoliniex)
  number <- which(names(Poliniex)==glue("USDT_{cryptocurrency}"))
  # To know if the platform has the cryptocurrency
  if (!is_empty(number)){
    PricePoliniex <- Poliniex[[number]]$last
  } else {
    PricePoliniex <- NA
  }
  df1 <- data.frame(
    Platform = "Poliniex",
    Price = round(as.numeric(PricePoliniex), digits = 2)
  )
  df <- rbind(df, df1)

  # Kucoin
  urlKucoin <- "https://api.kucoin.com/v1/open/currencies"
  Kucoin <- fromJSON(urlKucoin)
  if (!is.null(Kucoin[["data"]][["rates"]][[cryptocurrency]])){
    PriceKucoin <- Kucoin[["data"]][["rates"]][[cryptocurrency]]$USD
  } else {
    PriceKucoin <- NA
  }
  df1 <- data.frame(
    Platform = "Kucoin",
    Price = PriceKucoin
  )
  df <- rbind(df, df1)

  # Binance
  urlBinance <- "https://api.binance.com/api/v1/ticker/24hr"
  Binance <- fromJSON(urlBinance) %>%
    filter(symbol %in% glue("{cryptocurrency}USDT")) %>%
    select("lastPrice")
  df1 <- data.frame(
    Platform = "Binance",
    Price = as.numeric(Binance)
  )
  df <- rbind(df, df1)

  # Cryptopia
  urlCryptopia <- "https://www.cryptopia.co.nz/api/GetMarkets/USDT"
  Cryptopia <- fromJSON(urlCryptopia)$Data %>%
    filter(Label %in% glue("{cryptocurrency}/USDT")) %>%
    select("LastPrice")
  df1 <- data.frame(
    Platform = "Cryptopia",
    Price = round(as.numeric(Cryptopia), digits = 2)
  )
  df <- rbind(df, df1)

  return(df %>% arrange(desc(Price)))
}
