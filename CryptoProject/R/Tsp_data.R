#' Tsp_data
#'
#' small function use to create a dataframe for the use of the function lastweek_news_counter
#'
#' This function is used only to build the lastweek_news_counter function
#' @param timestamp date
#'
#' @return the dataframe retrieve from the api
#' @export Tsp_data
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#'
#' @examples
Tsp_data <- function(timestamp) {
  link <- glue("https://min-api.cryptocompare.com/data/v2/news/?lTs={timestamp}")
  data <- fromJSON(link)
  data2 <- data.frame("time" = data$Data$published_on, "body" = data$Data$body)
  return(data2)
}
