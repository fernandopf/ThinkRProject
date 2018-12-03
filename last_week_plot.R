library(CryptoProject)
library(plotly)

#TODO: check if x axis can become common

last_week_plot <- function(cryptocurrency= "BTC", comparison = "USD"){

  #calls for the news and prices dataframes from the last week
  df_news <- lastweek_news_counter(cryptocurrency)
  df_prices <- lastweek_minute(cryptocurrency, comparison)

  #plot of prices as a smoothed line of a generalised additive model
  p <- ggplot(df_prices %>% mutate(avg= (high+low)/2), aes(x= date, y= avg)) +
    geom_point(alpha= 0.9, size= 0.3, colour= "#0072B2") +
    stat_smooth(size= 0.5, colour= "red", method= "gam", formula = y~ s(x, bs= "cs"), se= FALSE)

  #plot of news as a barchart
  n <- plot_ly(data = df_news, x= ~time, y= ~mentioned, type ="bar")

  #return the two plots, combined verticaly
  return(subplot( ggplotly(p),
                  ggplotly(n),
                  nrows = 2))
}


last_week_plot("ETH","EUR")

#news as smoothed line
ggplot(news, aes(x= time, y= mentioned)) +
  stat_smooth(size= 0.5, colour= "gray", method= "gam", formula = y~ s(x, bs= "cs"), se= FALSE)
