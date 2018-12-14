#' Averages
#'
#' Function to  add financial indicators into the dataset
#'
#' This function has been designed to add financial indicators into the dataset (Moving average and MACD)
#'
#' @param df dataframe
#' @param n_MA Window Moving Average
#' @param n_quick_MACD  quick MACD
#' @param n_slow_MACD  slow MACD
#' @param n_signal_MACD signal MACD
#'
#' @return dataframe with the financial indicators added
#' @importFrom pracma movavg
#' @export
averages <- function(df, n_MA, n_quick_MACD, n_slow_MACD, n_signal_MACD){
  out <- df %>%
    mutate(daily_average = (high +low)/2)  %>%
    mutate(MA = movavg(x = daily_average, n = n_MA, type = "s")) %>%
    mutate(quick_EMA = movavg(x = daily_average, n = n_quick_MACD, type = "e")) %>%
    mutate(slow_EMA = movavg(x = daily_average, n = n_slow_MACD, type = "e")) %>%
    mutate(signal_MACD = movavg(x = daily_average, n = n_signal_MACD, type = "e")) %>%
    mutate(MACD = quick_EMA - slow_EMA)
  return(out)
}
