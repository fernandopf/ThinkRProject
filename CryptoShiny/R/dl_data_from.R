#' Downloads the news about cryptocurrencies from CryptoCompare API
#'
#' @param timestp a character of numerical value representing time
#' @return Dataframe with the time and body of the news released after timestamp
#' @example
#' \dontrun{
#' dl_data_from(as.numeric(as.POSIXct("2013-11-25 1:00:00 EST")))
#' }
#' @export dl_data_from

dl_data_from <- function(timestp) {
  new_data <- NULL
  fdata <- NULL
  timenow <- as.numeric(Sys.time())
  while (timestp < timenow)
  {
    new_data <- Tsp_data(timenow)
    fdata <- rbind(fdata, new_data)
    if (length(new_data$time) == 0) {
      print("no data recovered for this timestamp ")

    }
    timenow <- min(new_data$time)
    if (is.infinite(timenow)) {
      timenow <- timestp
    }
  }
  return(fdata)
}
