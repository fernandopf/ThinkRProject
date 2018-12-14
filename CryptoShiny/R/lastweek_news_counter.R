#' lastweek_news_counter
#'
#' Function to count the inputted crpytocurrency being mentioned hourly in last week
#'
#' This function is designed for giving a delayed indicator for the given crypto in the most recent week
#' @param cryptocurrency cryptocurrency
#'
#' @return dataframe with time and mentioned
#' @importFrom dplyr select filter mutate group_by summarise
#' @importFrom lubridate floor_date
#' @importFrom jsonlite fromJSON
#' @importFrom curl has_internet
#' @examples
#' \dontrun{
#' countNewsLastWeekBitcoin <- lastweek_news_counter("BTC")
#' }
#' @export


lastweek_news_counter <- function(cryptocurrency = "BTC") {
  # First we need to check that we have internet connection (needed to call the API)
  if (!has_internet()) {
    stop("You don't have internet connection")
  }

  #find the keywords
  currency.df <-
    fromJSON("https://api.hitbtc.com/api/2/public/currency")
  name <- currency.df %>%
    select(id, fullName) %>%
    filter(id == cryptocurrency)
  keywords <- paste(c(name$id, name$fullName), collapse = "|")
  #Build the dataset including news
  now <-
    round(as.numeric(as.POSIXct(Sys.time(), format = "%Y/%m/%d")))
  oneweek <-
    round(as.numeric(Sys.time() - as.difftime(7, units = "days")))

  data <-
    Tsp_data(now) #called the function Tsp_data inside this Package
  time <- min(data$time)
  timelimit <- oneweek
  while (time > timelimit)
  {
    new_data <- Tsp_data(time)
    time <- min(new_data$time)
    data <- rbind(data, new_data)
  }
  #find the match in the news
  count <- data %>%
    mutate(mentioned = as.numeric(grepl(
      pattern = keywords, body, ignore.case = TRUE
    ))) %>%
    mutate(time = as.POSIXct(time, origin = "1970-01-01", tz = "GMT")) %>%
    select(time, mentioned) %>%
    group_by(time = floor_date(time, "hour")) %>%
    summarise(mentioned = sum(mentioned))
  #return a dataframe with hourly datapoints
  return(count)
}
