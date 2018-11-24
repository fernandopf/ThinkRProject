crytocurrency_dataframe <- function(timeframe, firstDay, lastDay, crytocurrenty = "BTC", comparison = "USD") {
  
  # Initialitaion of the data frame with all the desired output
  df <- data.frame(
    Date=as.Date(character()),
    priceHighest=double(),
    priceLowest = double(),
    priceOpen = double(),
    priceClose = double()
  )
  
  # Date
  firstDay <- as.Date(firstDay,format="%d/%m/%Y") 
  lastDay <- as.Date(lastDay,format="%d/%m/%Y")
  time <- round(as.numeric(as.POSIXct(lastDay1, format="%m/%d/%Y")))

  # Number of points of hour dataframe
  n <- as.numeric(lastDay-firstDay)
  
  if (timeframe %in% c("Day", "day")){
    a <- "histoday"
  } 
  else if(timeframe %in% c("Minute", "minute")){
    a <- "histominute"
    n <- round(n*3600)
  } 
  else if (timeframe %in% c("Hour", "hour")) {
    a <- "histohour"
    n <- round(n*60)
  } 
  else {
    print('Valor no válido')
    return();
  }
  
  # Maximum number of points is 2000
  if (n <= 2000) {
    link <- paste("https://min-api.cryptocompare.com/data/", a,"?fsym=",crytocurrenty, "&tsym=", comparison,"&limit=", n, "&aggregate=1&toTs=",time, "&extraParams=your_app_name", sep = "")
    data <- fromJSON(link)
    df <- data.frame(
      Date= as.POSIXct(data$Data$time,origin = "1970-01-01",tz = "GMT"),
      high=data$Data$high,
      low = data$Data$low,
      open = data$Data$open,
      close = data$Data$close
    )
  }
  # If the number of points is higher than the maximum we need to do a for loop
  else {
  
  #for (n in 1:10){
  #  
  # Time <- actualTime - 3600*numberofpoint*n
  # link5 <- paste("https://min-api.cryptocompare.com/data/histohour?fsym=",cripto, "&tsym=", priceToCompare,"&limit=", numberofpoint, "&aggregate=1&toTs=",Time, "&extraParams=your_app_name", sep = "")
  # data5 <- fromJSON(link5)
  # 
  # df2 <- data.frame("Date" =as.POSIXct(data5$Data$time,origin = "1970-01-01",tz = "GMT"), "Price" = data5$Data$Price )
  # df <- rbind(df, df2)
  # }
    
  }
  return(df)
}