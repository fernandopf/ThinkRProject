#' Tsp_data
#'
#' small function use to create a dataframe for the use of the function lastweek_news_counter
#'
#' This function is used only to build the lastweek_news_counter function
#' @param timestp date
#'
#' @return the dataframe retrieve from the api
#' @importFrom glue glue
#' @importFrom curl has_internet
#' @importFrom jsonlite fromJSON
#' @example
#' \dontrun{
#' Tsp_data(round(as.numeric(as.POSIXct(Sys.time(), format="%Y/%m/%d"))))
#' }
#' @export

Tsp_data <- function(timestp) {

  # First we need to check that we have internet connection (needed to call the API)
  if (!has_internet()){
    stop("You don't have internet connection")
  }
  timestp <- floor(as.numeric(timestp))
  link <- glue("https://min-api.cryptocompare.com/data/v2/news/?lTs=", timestp, sep="")
  data <- fromJSON(link)
  data2 <- data.frame("time" = data$Data$published_on, "body" = data$Data$body)
  return(data2)
}
