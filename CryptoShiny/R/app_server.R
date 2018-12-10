#' app_server
#'
#' @param input input
#' @param output output
#' @param session session
#'
#' @return
#' @export app_server
#' @import shiny
#' @importFrom plotly renderPlotly
#' @importFrom DT renderDT
#' @importFrom glue glue
#'
#' @examples
app_server <- function(input, output, session){
  n <- 24
  # Anything that calls autoInvalidate will automatically invalidate
  # every 10 seconds.
  autoInvalidate <- reactiveTimer(10000)
  #-------------------first tab---------------------------------------------------------------




  #All parameters for 1st tab
  param1 <- reactiveValues(
    frame = "day",
    start = Sys.Date() - 90,
    end = Sys.Date() ,
    coin = "BTC",
    compare = "USD",
    ma = 5,
    quick_MA = 12,
    slow_MA = n,
    signal_MA = 9
  )

  observe({

    param1$coin <- input$currency
    param1$compare <- input$comparison
    param1$ma <- input$MA
    param1$slow_MA <- input$slow_MA
    param1$quick_MA <- input$quick_MA
    param1$signal_MA <- input$signal_MA

  })

  #Boundary for maximum number of points to compute the moving average
  observe({

    param1$timerange <- input$timerange
    param1$frame <- input$timeframe


    if (input$timeframe == "day"){


      updateNumericInput(session = session, inputId = "MA",
                         max =  as.numeric(input$timerange[2]-input$timerange[1]) -5)
      updateNumericInput(session = session, inputId = "slow_MA",
                         max = as.numeric(input$timerange[2]-input$timerange[1]) -5 )
      updateNumericInput(session = session, inputId = "quick_MA",
                         max = as.numeric(input$timerange[2]-input$timerange[1]) -5 )
      updateNumericInput(session = session, inputId = "signal_MA",
                         max = as.numeric(input$timerange[2]-input$timerange[1]) -5 )
    }
    else if (input$timeframe == "month"){

      updateNumericInput(session = session, inputId = "MA",
                         max = as.numeric(input$timerange[2]-input$timerange[1])/31 -5 )
      updateNumericInput(session = session, inputId = "slow_MA",
                         max = as.numeric(input$timerange[2]-input$timerange[1])/31 -5 )
      updateNumericInput(session = session, inputId = "quick_MA",
                         max = as.numeric(input$timerange[2]-input$timerange[1])/31 -5 )
      updateNumericInput(session = session, inputId = "signal_MA",
                         max = as.numeric(input$timerange[2]-input$timerange[1])/31 -5 )
    }
    else if (input$timeframe == "week"){

      updateNumericInput(session = session, inputId = "MA",
                         max = as.numeric(input$timerange[2]-input$timerange[1])/7 -5 )
      updateNumericInput(session = session, inputId = "slow_MA",
                         max = as.numeric(input$timerange[2]-input$timerange[1])/7 -5 )
      updateNumericInput(session = session, inputId = "quick_MA",
                         max = as.numeric(input$timerange[2]-input$timerange[1])/7 -5 )
      updateNumericInput(session = session, inputId = "signal_MA",
                         max = as.numeric(input$timerange[2]-input$timerange[1])/7 -5 )
    }
    else if (input$timeframe == "hour"){

      updateNumericInput(session = session, inputId = "MA",
                         max = as.numeric(input$timerange[2]-input$timerange[1])*24 -5 )
      updateNumericInput(session = session, inputId = "slow_MA",
                         max = as.numeric(input$timerange[2]-input$timerange[1])*24 -5 )
      updateNumericInput(session = session, inputId = "quick_MA",
                         max = as.numeric(input$timerange[2]-input$timerange[1])*24 -5 )
      updateNumericInput(session = session, inputId = "signal_MA",
                         max = as.numeric(input$timerange[2]-input$timerange[1])*24 -5 )
    }
  })

  #Options of timeframe when the time range is changed
  observe({
    #n = 24 because its a constraint in the statistics of slow_MA
    #If the timerange is bigger than 24 months: we can compute default slow_MA for month size data points
    if( as.numeric(input$timerange[2] - input$timerange[1]) > n * 30 ){
      updateSelectInput(session, inputId = "timeframe", label = "Choose first and last day: ",
                        selected = "month", choices = c("Month" = "month", "Week" = "week", "Day" = "day") )
    }
    #If the timerange is bigger than 24 weeks: we can compute default slow_MA for week size data points
    else if( as.numeric(input$timerange[2] - input$timerange[1]) > n * 7 ){
      updateSelectInput(session, inputId = "timeframe", label = "Choose first and last day: ",
                        selected = "week", choices = c( "Week" = "week", "Day" ="day") )
    }
    #If the timerange is bigger than 24 days: we can compute default slow_MA for day size data points
    else if( as.numeric(input$timerange[2] - input$timerange[1]) > n ){
      updateSelectInput(session, inputId = "timeframe", label = "Choose first and last day: ",
                        selected = "day", choices = c( "Day" = "day") )
    }
    #If the timerange is bigger than 24 hours: we can compute default slow_MA for hour size data points
    else {
      updateSelectInput(session, inputId = "timeframe", label = "Choose first and last day: ",
                        selected = "hour", choices = c("Hour" = "hour") )
    }
  })

  #------------------------------------------------------------------------------------

  #First tab default plot: Currency prices, moving averages,...; default MACD
  output$mainPlot <- renderPlotly({

    frame <- param1$frame
    start <- param1$timerange[1]
    end <- param1$timerange[2]
    coin <- param1$coin
    compare <- param1$compare
    ma <- param1$ma
    slow_ma <- param1$slow_MA
    quick_ma <- param1$quick_MA
    signal_ma <- param1$signal_MA

    df <- crypto(timeframe = frame, firstDay = format(start, "%d/%m/%Y"),
                 lastDay = format(end, "%d/%m/%Y"),
                 cryptocurrency = coin, comparison = compare,
                 n_MA = ma, n_quick_MACD = quick_ma, n_slow_MACD = slow_ma, n_signal_MACD = signal_ma)



    candle_plot(df, MACD)
  })


  #Transforming plot when MACD or VOLUME tab selected
  observe({
    input$Run_tab1   #Only change plot when user clicks on Run APP button

    isolate({

      frame <- param1$frame
      start <- param1$timerange[1]
      end <- param1$timerange[2]
      coin <- param1$coin
      compare <- param1$compare
      ma <- param1$ma
      slow_ma <- param1$slow_MA
      quick_ma <- param1$quick_MA
      signal_ma <- param1$signal_MA

      #Change main plot if user choose MACD
      if (input$tabset == "MACD") {

        output$mainPlot <- renderPlotly({

          df <- crypto(timeframe = frame, firstDay = format(start, "%d/%m/%Y"),
                       lastDay = format(end, "%d/%m/%Y"),
                       cryptocurrency = coin, comparison = compare,
                       n_MA = ma, n_quick_MACD = quick_ma, n_slow_MACD = slow_ma, n_signal_MACD = signal_ma)

          candle_plot(df, MACD)

        }) #End of render
      } #End of if

      #Change main plot if user choose VOLUME
      else if (input$tabset == "VOLUME") {

        output$mainPlot <- renderPlotly({

          df <- crypto(timeframe = frame, firstDay = format(start, "%d/%m/%Y"),
                       lastDay = format(end, "%d/%m/%Y"),
                       cryptocurrency = coin, comparison = compare,
                       n_MA = ma, n_quick_MACD = quick_ma, n_slow_MACD = slow_ma, n_signal_MACD = signal_ma)

          candle_plot(df, volume)
        }) #End of render
      }#End of else if
    }) #End of isolate

    #Print the latest price
    output$latestprice <- renderText({
      autoInvalidate()
      temp <- lastweek_minute(coin, compare, only_two_minutes = TRUE)
      glue("<font color=\"#FF0000\"><TT><font size=10>{temp[2,2]}</TT></font> <TT><font size=3>{coin}/{compare}</TT></font>    <p><TT><font size=3>update time: {as.character(Sys.time())}</TT></font>")
    })

  }) #End of observer for plot transformation due to tab selection

  #-------------------------start of the server part of the 2nd tab---------------------------------

  #All parameters for 2st tab
  param2 <- reactiveValues(
    grouping = "day",
    cryptocurrency = "BTC",
    comparison = "USD"
  )

  observe({
    input$Run_tab2   #Only change plot when user clicks on Run APP button

    isolate({
      param2$cryptocurrency <- input$currency2
      param2$comparison <- input$comparison2
      param2$grouping <- input$grouping
    })
  })

  #------------------------------------------------------------------------------------

  #Second tab plot: Currency prices from last week per minute; News count
  output$lastweekPlot <- renderPlotly({


    lastweekplot <- plot_lastweek(cryptocurrency = param2$cryptocurrency,
                                  comparison = param2$comparison, grouping = param2$grouping)

    lastweekplot
  })


  #-------------------third tab---------------------------------------------------------------
  #All parameters for 3rd tab
  param3 <- reactiveValues(
    cryptoID = "BTC"
  )

  observe({
    input$Run_tab3   #Only change plot when user clicks on Run APP button
    isolate({
      param3$cryptoID <- input$cryptoID
    })
  })

  #------------------------------------------------------------------------------------

  #table: compare different price on different online platforms
  output$pricetable <- renderDT({
    pricecomparison <- getLastPriceMultiplePlatforms(param3$cryptoID)
    pricecomparison
  })
  output$averageprice <- renderText({
    pricecomparison <- getLastPriceMultiplePlatforms(param3$cryptoID)
    meanprice <- round(mean(as.numeric(pricecomparison$Price), na.rm = TRUE), digits = 4)
    glue("<font color=\"#FF0000\"><TT><font size=3>The average price is</TT></font> <TT><font size=10>{meanprice}</TT></font> <TT><font size=3>{param3$cryptoID}/USD</TT></font>   <p><TT><font size=3>update time: {as.character(Sys.time())}</TT></font>")
  })



} #End of server---------
