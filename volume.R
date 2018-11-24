crytocurrency_dataframe <- function(firstDay, lastDay, crytocurrenty = "BTC", comparison = "USD") {
  library(dplyr)
  # Initialitaion of the data frame with all the desired output
  df <- data.frame(
    Date=as.Date(character()),
    Volume=double(),
    )
  
  # Date
  firstDay <- as.Date(firstDay,format="%d/%m/%Y") 
  lastDay <- as.Date(lastDay,format="%d/%m/%Y")
  time <- round(as.numeric(as.POSIXct(lastDay1, format="%m/%d/%Y")))
  
  # Number of points of hour dataframe
  n <- as.numeric(lastDay-firstDay)
  incr <- 3600*24
  
  if (timeframe %in% c("Day", "day")){
    a <- "histoday"
  } 
  else if(timeframe %in% c("Minute", "minute")){
    a <- "histominute"
    incr <- 60
    n <- round(n*3600)
    # As limit, we only can get the minutes from the last week
    timeLimit <- as.numeric(Sys.time()-as.difftime(7, unit="days"))
    # if (timeLimit ){
    #   print("Minute data  is only available for the last 7 days")
    # }
  } 
  else if (timeframe %in% c("Hour", "hour")) {
    a <- "histohour"
    n <- round(n*60)
    incr <- 3600
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
    # Round to the highest Integuer
    iterations <- ceiling(n/2000)
    n1 <- 2000
    for (i in 1:iterations){
      if (i ==iterations){
        n1 =n-2000*(iterations-1) 
      }
      link <- paste("https://min-api.cryptocompare.com/data/", a,"?fsym=",crytocurrenty, "&tsym=", comparison,"&limit=", n1, "&aggregate=1&toTs=",time, "&extraParams=your_app_name", sep = "")
      data <- fromJSON(link)
      df1 <- data.frame(
        Date= as.POSIXct(data$Data$time,origin = "1970-01-01",tz = "GMT"),
        high=data$Data$high,
        low = data$Data$low,
        open = data$Data$open,
        close = data$Data$close
      )
      df <- rbind(df, df1)
      time <- time - 2000*incr
    }
  }
  df <-df %>% mutate(direction = ifelse(high >low, "increasing", "decreasing")) %>% arrange(Date)
  return(df)
}