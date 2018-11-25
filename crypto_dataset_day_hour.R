crypto_dataset_day_hour <- function(timeframe, firstDay, lastDay, crytocurrenty = "BTC", comparison = "USD") {
  library(dplyr)
  library(jsonlite)

  # Date
  firstDay <- as.Date(firstDay,format="%d/%m/%Y") 
  lastDay <- as.Date(lastDay,format="%d/%m/%Y")
  
  time <- round(as.numeric(as.POSIXct(lastDay, format="%m/%d/%Y")))
  
  # Number of points of hour dataframe
  n <- as.numeric(lastDay-firstDay)
  
  if (timeframe %in% c("Day", "day")){
    a <- "histoday"
    incr <- 3600*24
  } 
  else if (timeframe %in% c("Hour", "hour")) {
    a <- "histohour"
    n <- round(n*24)
    incr <- 3600
  } 
  else {
    print('No valid timeframe')
    return();
  }
  
  # Maximum number of points is 2000
  if (n <= 2000) {
    link <- paste("https://min-api.cryptocompare.com/data/", a,"?fsym=",crytocurrenty, "&tsym=", comparison,"&limit=", n, "&aggregate=1&toTs=",time, "&extraParams=ThinkR", sep = "")
    linkVolume <- paste("https://min-api.cryptocompare.com/data/exchange/", a,"?tsym=",crytocurrenty,"&limit=", n, "&toTs=",time, "&extraParams=ThinkR", sep = "")
    dataPrice <- fromJSON(link)
    dataVolume <- fromJSON(linkVolume)
    df <- data.frame(
      Date= as.POSIXct(dataPrice$Data$time,origin = "1970-01-01",tz = "GMT"),
      high=dataPrice$Data$high,
      low = dataPrice$Data$low,
      open = dataPrice$Data$open,
      close = dataPrice$Data$close,
      volume = dataVolume$Data$volume
    )
  }
  # If the number of points is higher than the maximum we need to do a for loop
  else {
    # Round to the highest Integuer
    iterations <- ceiling(n/2000)
    n1 <- 2000
    for (i in 1:iterations){
      if (i ==iterations){
        n1 =n-2000*(iterations-1) 
      }
      linkPrice <- paste("https://min-api.cryptocompare.com/data/", a,"?fsym=",crytocurrenty, "&tsym=", comparison,"&limit=", n1, "&aggregate=1&toTs=",time, "&extraParams=ThinkR", sep = "")
      linkVolume <- paste("https://min-api.cryptocompare.com/data/exchange/", a,"?tsym=",crytocurrenty,"&limit=", n1, "&toTs=",time, "&extraParams=ThinkR", sep = "")
      dataPrice <- fromJSON(linkPrice)
      dataVolume <- fromJSON(linkVolume)
      df1 <- data.frame(
        Date= as.POSIXct(dataPrice$Data$time,origin = "1970-01-01",tz = "GMT"),
        high=dataPrice$Data$high,
        low = dataPrice$Data$low,
        open = dataPrice$Data$open,
        close = dataPrice$Data$close,
        volume = dataVolume$Data$volume
      )
      if (i ==1){
        df <- df1
      } else {
        df <- rbind(df, df1)
      }
      time <- time - 2000*incr
    }
  }
  df <-df %>% mutate(direction = ifelse(open >close, "increasing", "decreasing")) %>% arrange(Date)
  return(df)
}