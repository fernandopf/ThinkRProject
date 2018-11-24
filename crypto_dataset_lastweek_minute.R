crypto_dataset_lastweek_minute <- function(crytocurrenty = "BTC", comparison = "USD") {
  library(dplyr)
  library(jsonlite)
  
  # Initialitaion of the data frame with all the desired output
  df <- data.frame(
    Date=as.Date(character()),
    high=double(),
    low = double(),
    open = double(),
    close = double()
  )
  
  actualTime <- round(as.numeric(Sys.time()))
  MaxLimit <- as.numeric(Sys.time()-as.difftime(7, unit="days"))
  
  # Number of points of  dataframe
  n <- round(actualTime-MaxLimit)/60
  
  # Round to the highest Integuer
  iterations <- ceiling(n/2000)
  
  # Set the time equal to the actual
  time = actualTime
  n1 <- 2000
  for (i in 1:iterations){
    if (i ==iterations){
      n1 =n-2000*(iterations-1) 
    }
    link <- paste("https://min-api.cryptocompare.com/data/histominute?fsym=",crytocurrenty, "&tsym=", comparison,"&limit=", n1, "&aggregate=1&toTs=",time, "&extraParams=your_app_name", sep = "")
    data <- fromJSON(link)
    df1 <- data.frame(
      Date= as.POSIXct(data$Data$time,origin = "1970-01-01",tz = "GMT"),
      high=data$Data$high,
      low = data$Data$low,
      open = data$Data$open,
      close = data$Data$close
    )
    df <- rbind(df, df1)
    time <- time - 2000*60
  }
  
  df <-df %>% mutate(direction = ifelse(open >close, "increasing", "decreasing")) %>% arrange(Date)
  return(df)
}