#' Plot trend and citations for last week
#'
#' A plot exhibitng the trend of the exchange rate between the currencies chosen is displayed.
#' Supplementary, the number of citations referring to the chosen currency from selected news sources is shown.
#'
#' @param cryptocurrency the currency to examined
#' @param comparison the currency over which, the exchange rate will be shown
#' @param grouping the citations can be grouped in terms of "day", "12 hours", "6 hours", "3 hours", "hour".
#'
#' @return 2 plots. One of the exchange rate and one with the number of citations.
#' @importFrom dplyr group_by summarise mutate
#' @importFrom lubridate floor_date
#' @importFrom ggplot2 ggplot aes geom_point stat_smooth labs theme_bw theme element_blank geom_col geom_smooth
#' @importFrom plotly subplot ggplotly
#' @export
#'
#' @seealso \code{\link{candle_plot}}
#'
#' @examples
#' \dontrun{
#'
#' Call with default values:
#' plot_lastweek()
#'
#' Specifying the currencies for the display:
#' plot_lastweek(cryptocurrency= "ETH", comparison= "EUR")
#'
#' Specifying the grouping of the citations:
#' plot_lastweek(grouping= "6 hours")
#'
#' }
#'
plot_lastweek <- function(cryptocurrency= "BTC", comparison = "USD", grouping= "day"){

  #calls for the news and prices dataframes from last week
  df_news <- lastweek_news_counter(cryptocurrency)
  df_news <- df_news %>%
    group_by(time = floor_date(time, unit = grouping)) %>%
    summarise(mentioned = sum(mentioned))
  df_prices <- lastweek_minute(cryptocurrency, comparison)
  df_prices <- df_prices %>% mutate(avg= (high+low)/2)

  #plot of prices as a smoothed line of a generalised additive model
  p <- ggplot(df_prices %>% mutate(avg= (high+low)/2), aes(x= date, y= avg)) +
    geom_point(alpha= 0.6, size= 0.3, colour= "#0072B2") +
    stat_smooth(size= 0.5, colour= "red", method= "gam", formula = y~ s(x, bs= "cs"), se= FALSE) +
    labs(x= "Time", y= "Exchange Rate") +
    theme_bw() +
    theme(panel.border= element_blank())

  #plot of news as a barchart
  n <- ggplot(df_news, aes(x= time, y= mentioned)) +
    geom_col(alpha=0.5) +
    geom_smooth(size= 0.3, colour= "blue", method= "loess", formula = y~x, se= FALSE) +
    labs(title= "Trend of Currency Last Week", x= "Time", y= "Citations") +
    theme_bw() +
    theme(panel.border= element_blank())

  #return combined plot
  return(subplot( ggplotly(p),
                  ggplotly(n),
                  nrows = 2,
                  margin = 0.01,
                  heights = c(0.8,0.2),
                  shareX = TRUE,
                  titleY = TRUE
  ))
}
