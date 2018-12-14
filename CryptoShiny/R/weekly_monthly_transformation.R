#' weekly_monthly_transformation
#'
#' Function to transform the timeframe of the dataset
#'
#' This function has been designed to transform  the timeframe (monthly or weekly) of the dataset returned by the function day_hour.R
#' @param df dataframe to transform
#' @param timeframe month or day
#'
#' @return dataframe with the time, highest price, lowest price, open price, close price in the chosen timeframe
#' @return dataframe transformed
#' @importFrom dplyr group_by summarise
#' @importFrom lubridate floor_date
#' @importFrom utils head tail
#'
#' @examples
#' \dontrun{
#' weekly_monthly_transformation(bitcoinVsDollarExampleDay, "week")
#' }
#' @export weekly_monthly_transformation

weekly_monthly_transformation <- function(df, timeframe){
  if (timeframe %in% c("Month", "month")){
    df.transformed <- df %>%
      group_by(date = floor_date(date, "month")) %>%
      summarise(high = max(high), low = min(low), open = head(open, n = 1), close = tail(close, n = 1), volume = sum(volume), news = sum(news)) %>%
      mutate(direction = ifelse(open > close, "increasing", "decreasing"))
  }
  else if (timeframe %in% c("Week", "week")){
    df.transformed <- df %>%
      group_by(date = floor_date(date, "week")) %>%
      summarise(high = max(high), low = min(low), open = head(open, n = 1), close = tail(close, n = 1), volume = sum(volume), news = sum(news)) %>%
      mutate(direction = ifelse(open > close, "increasing", "decreasing"))
  }
  return(df.transformed)
}
