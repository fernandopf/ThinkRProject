#' Candle plot
#'
#'Function to make an interactive plot
#'
#'This function will return a plot about the prices of a certain coin along the time as well as some statistics such as the mean, the moving average; the volume or MACD
#'
#' @param data dataframe with the information
#' @param supp_type name of the column that has the supplementary information (volume or MACD)
#'
#' @return plotly object
#' @importFrom plotly plot_ly layout add_lines subplot
#' @importFrom rlang enquo
#' @importFrom stats as.formula
#' @example
#' \dontrun{
#' candle_plot(data= exampleCryptoBTCUSDHour, MACD)
#'}
#' @export
#'
#' @examples
candle_plot <- function(data, supp_type) {
  #Careful with NSE!!
  supp_type <- enquo(supp_type)

  #Colors depending on if price is increasing or decreasing
  increase <- list(line = list(color = 'green'))
  decrease <- list(line = list(color = 'red'))

  #Main plot: Candlestick plot
  p1 <- data %>%
    plot_ly(
      x = ~ date,
      type = "candlestick",
      open = ~ open,
      close = ~ close,
      high = ~ high,
      low = ~ low,
      name = "Stock price",
      increasing = increase,
      decreasing = decrease
    ) %>% #Setting colors for increasing or decreasing stock
    #Daily average line
    add_lines(
      x = ~ date,
      y = ~ daily_average ,
      name = "Daily avg",
      line = list(color = 'black', width = 1),
      hoverinfo = "none",
      inherit = F
    ) %>%
    #Moving Average line
    add_lines(
      x = ~ date,
      y = ~ MA ,
      name = "MA50",
      line = list(color = 'red', width = 1),
      hoverinfo = "none",
      inherit = F
    ) %>%
    #Layout of plot
    layout(yaxis = list(title = "Price", fixedrange = FALSE),
           title = "Info")

  #Supplementary plot: depending on supp_type parameter
  p2 <- data %>%
    plot_ly(
      x =  ~ date,
      y = as.formula(supp_type),
      type = 'bar',
      name = "supp",
      color = ~ direction,
      colors = c('green', 'red')
    ) %>%
    layout(yaxis = list(fixedrange = FALSE))

  #Merging both plots into one interacting plot
  p <- subplot(
    p1,
    p2,
    heights = c(0.7, 0.2),
    nrows = 2,
    shareX = TRUE,
    titleY = TRUE
  ) %>%
    layout(
      title = "Info",
      legend = list(
        orientation = 'h',
        x = 0.5,
        y = 1,
        xanchor = 'center',
        font = list(size = 10)
      )
    )
  return(p)
}
