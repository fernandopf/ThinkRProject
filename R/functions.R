#' Downloads the news about cryptocurrencies from CryptoCompare API
#'
#' @param timestamp a character of numerical value representing time
#' @return Dataframe with the time and body of the 50 latests news released before timestamp
#' @examples
#' Tsp_data(1543920423)
Tsp_data <- function(timestamp) {
  link <- paste("https://min-api.cryptocompare.com/data/v2/news/?lTs=", floor(timestamp), sep="")
  ndata <- fromJSON(link)
  data2 <- data.frame("time" = as.integer(ndata$Data$published_on), "body" = as.character(ndata$Data$body))
  return(data2)
}

#' Downloads the news about cryptocurrencies from CryptoCompare API
#'
#' @param timestamp a character of numerical value representing time
#' @return Dataframe with the time and body of the news released after timestamp
#' @examples
#' dl_data_from(1543820423)
dl_data_from <- function(timestp) {
  new_data <- NULL
  fdata <- NULL
  timenow <- as.numeric(Sys.time())
  while (timestp < timenow)
  {
    new_data <- Tsp_data(timenow)
    fdata <- rbind(fdata,new_data)
    if (length(new_data$time) == 0) {
      print("no data recovered for timestamp ")
      print(timenow)
    }
    timenow <- min(new_data$time)
  }
  return(fdata)
}

#' Returns the name of a cryptocurrency using its symbol. Uses the dataframe crp_dic.csv
#'
#' @param CryptoSymbol a character representing the cryptocurrency Symbol
#' @return Character representing the name of the wanted cryptocurrency
#' @examples
#' CryptoCurrencyName("BTC")
CryptoCurrencyName <- function(CryptoSymbol) {
  dico <- read.csv2("crp_dic.csv")
  Symb <- dico %>% filter(Symbol == CryptoSymbol)
  return(as.character(Symb$Name))
}

#' Analyses a dataframe with news body for occurences of the currencySymbol
#'
#' @param databodynews dataframe with news body and their timestamp
#' @param CurrencySymbol a character representing the cryptocurrency Symbol
#' @return Dataframe with a boolean column representing if the cryptocurrency was cited in the news article
#' @examples
#' analyse_crp_news(datanews, "BTC")
analyse_crp_news <- function(databodynews, currencySymbol) {
  currencyName <- CryptoCurrencyName(currencySymbol)
  analysed <- databodynews %>% mutate(!!currencySymbol :=  ifelse(grepl(as.character(currencyName),body,ignore.case = TRUE), 1, 0) )
  return(analysed)
}

#' Analyses a dataframe with news body for occurences of the each of the currencySymbols
#'
#' @param databodynews dataframe with news body and their timestamp
#' @param CurrencySymbols a vector of characters representing the cryptocurrencys Symbols to be analysed
#' @return Dataframe with a boolean column per cryptosymbol, representing if the cryptocurrency was cited in the news article
#' @examples
#' analyse_crps_news(datanews, c("BTC", "ETH"))
analyse_crps_news <- function(databodynews, currencySymbols) {
  resul <- analyse_crp_news(databodynews, currencySymbols[1])[-2]
  for (i in 2:length(currencySymbols))
    resul <- cbind(resul, analyse_crp_news(databodynews, currencySymbols[i])[3])
  return(resul)
}

#' Analyses a dataframe of news article timestamp and occurences of currencySymbols and sums the number of occurences of each currency during the hour specified in argument
#'
#' @param BoolDataNews dataframe of news article timestamp and boolean occurences of currencySymbols
#' @param CrpSymbols a vector of characters representing the cryptocurrencys Symbols to be analysed
#' @param hourTstp the timestamp of the hour to be analysed
#' @return Dataframe with a counter column per cryptosymbol, representing the number of times each cryptocurrency was cited in the news articles during the hour
#' @examples
#' SumlastHour(1543820423, c("BTC", "ETH"), booldatanews)
SumlastHour <- function(hourTstp, CrpSymbols, BoolDataNews) {
  res <- BoolDataNews %>% filter(time<hourTstp, time>hourTstp-3600)
  resu <- hourTstp
  for (i in 2:length(res)) {
    resa <- res %>% filter(res[i] == 1)
    resu <- cbind(resu, length(resa$time))
  }
  resu <- data.frame(resu)
  names(resu) <- c("time", CrpSymbols)
  return(resu)
}

#' Analyses a dataframe of news article timestamp and occurences of currencySymbols and sums the number of occurences of each currency per hour
#'
#' @param resTsp dataframe of news article timestamp and boolean occurences of currencySymbols
#' @param CrpSymbols a vector of characters representing the cryptocurrencys Symbols to be analysed
#' @return Dataframe with a counter column per cryptosymbol, representing the number of times each cryptocurrency was cited in the news articles diring the hour
#' @examples
#' resTspToHour(booldatanews, c("BTC", "ETH"))
resTspToHour <- function(resTsp, CrpSymbols) {
  max <- floor_date(as.POSIXct(max(resTsp$time), origin="1970-01-01"), unit = "hour") + 3600
  min <- floor_date(as.POSIXct(min(resTsp$time), origin="1970-01-01"), unit = "hour")

  hourvec <- seq(max, min, by=-3600)
  resul <- NULL
  for (hour in hourvec) {
    resul <- rbind(resul, SumlastHour(hour, CrpSymbols, resTsp))
  }
  return(resul %>% mutate(time = as.POSIXct(time, origin="1970-01-01")))
}

#' Analyses a dataframe of news article timestamp and occurences of currencySymbols and sums the number of occurences of each currency during the day specified in argument
#'
#' @param BoolDataNews dataframe of news article timestamp and boolean occurences of currencySymbols
#' @param CrpSymbols a vector of characters representing the cryptocurrencys Symbols to be analysed
#' @param dayTstp the timestamp of the hour to be analysed
#' @return Dataframe with a counter column per cryptosymbol, representing the number of times each cryptocurrency was cited in the news articles during the hour
#' @examples
#' SumlastDay(1543820423, c("BTC", "ETH"), booldatanews)
SumlastDay <- function(dayTstp, CrpSymbols, BoolDataNews) {
  res <- BoolDataNews %>% filter(time<dayTstp, time>dayTstp-3600*24)
  resu <- dayTstp
  for (i in 2:length(res)) {
    resa <- res %>% filter(res[i] == 1)
    resu <- cbind(resu, length(resa$time))
  }
  resu <- data.frame(resu)
  names(resu) <- c("time", CrpSymbols)
  return(resu)
}

#' Analyses a dataframe of news article timestamp and occurences of currencySymbols and sums the number of occurences of each currency per day
#'
#' @param resTsp dataframe of news article timestamp and boolean occurences of currencySymbols
#' @param CrpSymbols a vector of characters representing the cryptocurrencys Symbols to be analysed
#' @return Dataframe with a counter column per cryptosymbol, representing the number of times each cryptocurrency was cited in the news articles diring the hour
#' @examples
#' resTspToDay(booldatanews, c("BTC", "ETH"))
resTspToDay <- function(resTsp, CrpSymbols) {
  max <- floor_date(as.POSIXct(max(resTsp$time), origin="1970-01-01"), unit = "day") + 3600*24
  min <- floor_date(as.POSIXct(min(resTsp$time), origin="1970-01-01"), unit = "day")

  dayvec <- seq(max, min, by=-3600*24)
  resul <- NULL
  for (day in dayvec) {
    resul <- rbind(resul, SumlastDay(day, CrpSymbols, resTsp))
  }
  return(resul %>% mutate(time = as.POSIXct(time, origin="1970-01-01")))
}

#' Downloads the 20 cryptocurrencies that have the highest market capitalization from cryptocompare API
#'
#' @return vector of characters representing the symbols of the cryptocurencies
#' @examples
#' get_imp_Crp()
get_imp_Crp <- function(){
  ndata <- fromJSON("https://min-api.cryptocompare.com/data/top/volumes?tsym=USDT")
  c(ndata$Data$SYMBOL,"USDT")
}


#' Updates the dataset CryptoNewsAnalysedDays with the latest news from CryptoCompare API
#'
#' @examples
#' updateDayNewsData()
updateDayNewsData <- function() {
  dataDay <- read.csv("data/CryptoNewsAnalysedDays.csv")[-1]
  dataDay$time <- as.POSIXct(dataDay$time)
  newestDay <- max(dataDay$time)
  Newdata <- dl_data_from(as.numeric(newestDay))
  if (length(Newdata$time) != 0) {
    Newdata <- Newdata %>% filter(time > newestDay)
    if (length(Newdata$time) != 0) {
      interesting_cryptos <- names(dataDay)[-1]
      result <- analyse_crps_news(Newdata, interesting_cryptos)
      finalDay <- resTspToDay(result, interesting_cryptos)
      finalDay <- finalDay %>% filter(time != min(time)) #avoid duplicates
      total <- rbind(finalDay, dataDay)
      write.csv(total, "data/CryptoNewsAnalysedDays.csv")
      print("updated")
    }
  }
  if (length(Newdata$time) == 0) {
    print("Already up to date")
  }
}

#' Updates the dataset CryptoNewsAnalysedHours with the latest news from CryptoCompare API
#'
#' @examples
#' updateHourNewsData()
updateHourNewsData <- function() {
  dataHour <- read.csv("data/CryptoNewsAnalysedHour.csv")[-1]
  dataHour$time <- as.POSIXct(dataHour$time)
  newestHour <- max(dataHour$time)
  Newdata <- dl_data_from(as.numeric(newestHour))
  if (length(Newdata$time) != 0) {
    Newdata <- Newdata %>% filter(time > newestHour)
    if (length(Newdata$time) != 0) {
      interesting_cryptos <- names(dataHour)[-1]
      result <- analyse_crps_news(Newdata, interesting_cryptos)
      finalHour <- resTspToHour(result, interesting_cryptos)
      finalHour <- finalHour %>% filter(time != min(time)) #avoid duplicates
      total <- rbind(finalHour, dataHour)
      write.csv(total, "data/CryptoNewsAnalysedHour.csv")
      print("updated")
    }
  }
  if (length(Newdata$time) == 0) {
    print("Already up to date")
  }
}
