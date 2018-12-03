#' Crypto
#'
#' Master function
#'
#' Master function which aggragate all the function of our package
#'
#' @param timeframe timeframe in hour or day
#' @param firstDay first day to analyse in dd/mm/yyyy format
#' @param lastDay last day to analyse in dd/mm/yyyy format
#' @param cryptocurrency cryptocurrency to analyse
#' @param comparison currency to be compared
#' @param n_MA Window Moving Average
#' @param n_quick_MACD quick MACD
#' @param n_slow_MACD slow MACD
#' @param n_signal_MACD signal MACD
#'
#' @return dataframe with the time, highest price, lowest price, open price, close price and financial indicators in the chosen timeframe
#' @export
#'
#' @examples
crypto <- function(timeframe, firstDay, lastDay, cryptocurrency = "BTC", comparison = "USD", n_MA, n_quick_MACD, n_slow_MACD, n_signal_MACD){
  if (timeframe %in% c("Week", "week", "Month", "month")){
    df <- day_hour(timeframe = "day", firstDay, lastDay, cryptocurrency, comparison);
    df.transformed <- weekly_monthly_transformation(df, timeframe);
    df.averaged <- averages(df.transformed, n_MA, n_quick_MACD, n_slow_MACD, n_signal_MACD)
  }
  else {
    df <- day_hour(timeframe, firstDay, lastDay, cryptocurrency, comparison);
    df.averaged <- averages(df,n_MA, n_quick_MACD, n_slow_MACD, n_signal_MACD)
  }
  return(df.averaged)
}
