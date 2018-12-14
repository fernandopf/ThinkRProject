#' Updates the dataset CryptoNewsAnalysedDays with the latest news from CryptoCompare API
#'
#' @return updated dataframe
#' @importFrom dplyr filter mutate
#' @importFrom glue glue
#' @importFrom utils read.csv write.csv
#' @example
#' \dontrun{
#' updateDayNewsData()
#' }
#' @export

updateDayNewsData <- function() {
  dataDay <- CryptoNewsOccurencesDays
  dataDay$time <- as.POSIXct(dataDay$time)
  newestDay <- max(dataDay$time)
  print(
    glue(
      "News by day Data loaded, number of entries: ",
      length(dataDay$time),
      ", last entry from: ",
      newestDay,
      sep = ""
    )
  )
  Newdata <- dl_data_from(as.numeric(newestDay))
  if (length(Newdata$time) != 0) {
    Newdata <- Newdata %>%
      filter(time > newestDay)
    if (length(Newdata$time) != 0) {
      interesting_cryptos <- names(dataDay)[-1]
      result <- analyse_crps_news(Newdata, interesting_cryptos)
      finalDay <- resTspToDay(result, interesting_cryptos)
      finalDay <- finalDay %>%
        filter(time != min(time)) #avoid duplicates
      total <- rbind(finalDay, dataDay)
      CryptoNewsOccurencesDays <- total
      print(
        glue(
          "Dataset updated: number of entries: ",
          length(total$time),
          ", last entry from: ",
          max(total$time),
          sep = ""
        )
      )
    }
  }
  if (length(Newdata$time) == 0) {
    print("Already up to date")
  }
  return(CryptoNewsOccurencesDays)
}
