
library(shiny)
library(shinythemes)
library(markdown)
library(CryptoProject)
library(plotly)
library(shinydashboard) #Make tabs as buttons
library(DT)

#Reading all possible currencies from CSV
name_coins <- read.csv("data/coins.csv", header = FALSE)[,1]
all_coins <- as.list(name_coins)
names(all_coins) <- name_coins

#User interface
ui <- fluidPage(
   #Theme
   theme = shinytheme("sandstone"),
   
   # Application title
   titlePanel("Cryptocurrency Project"),
  
   navbarPage("CrytoProject!",
    #-------------------------------1st tabPanel--------------------------------------
      tabPanel("Historical Price",
               
        sidebarLayout(
          #Sidebar Layout-----------------
          sidebarPanel(
            
            #Currency we are interested in
            selectInput(inputId = "currency", label = "Currency you are interested? ",
                        selected = "BTC", choices = all_coins ),
            
            #Currency we want to compare with
            selectInput(inputId = "comparison", label = "Currency you want to compare? ",
                        selected = "USD", choices = all_coins ),
            
            #Time range we we are interested in
            dateRangeInput(inputId = "timerange", 
                           label = "Choose first and last day: ",
                           start = Sys.Date() - 90, end = Sys.Date(),
                           separator = " - ", format = "dd/mm/yy",
                           startview = 'year', language = 'fr', weekstart = 1),
            
            #Size of a point: hour, days, weeks or month
            selectInput(inputId = "timeframe", label = "Choose first and last day: ",
                        selected = "day", choices = c("Day" = "day") ) ,
            
            #Number of points used to compute moving average
            numericInput( inputId= "MA",
                          label = "Moving Average (MA)",
                          value = 5, min = 2, max = 85, step = 1),
          
            #Type of supplementary plot: MACD or volume
            tags$b("Choose type supplementary plot:" ),
            
            fluidRow(
                tabBox( id = "tabset", selected = "MACD", side = "left",
                        tabPanel(title = "MACD", value = "MACD",
                                 
                                 #Number of points used to compute the slow moving average
                                 numericInput( inputId= "slow_MA",
                                               label = "Slow EMA",
                                               value = 24, min = 2, max = 85, step = 1),
                                 
                                 #Number of points used to compute quick moving average
                                 numericInput( inputId= "quick_MA",
                                               label = "Quick EMA",
                                               value = 12, min = 2, max = 85, step = 1),
                                 
                                 #Number of points used to compute signal moving average
                                 numericInput( inputId= "signal_MA",
                                               label = "Signal MACD",
                                               value = 9, min = 2, max = 85, step = 1)
                                 ),
                        
                          tabPanel(title = "VOLUME", value = "VOLUME")
                        
                      ) #End of tabBox
                    )

          ), #End of Sidebar Panel-----------------
          
          #Main Panel: Plot
          mainPanel(
            htmlOutput("latestprice"),
            plotlyOutput("mainPlot")
         )
      ) #End of Sidebar Layout
      
    ), #End of first tab
    
    #------------------------------------2nd tabPanel-------------------------------------
      tabPanel("Minutely Prices", 
               
        sidebarLayout(
            #Sidebar Layout-----------------
            sidebarPanel(
               #Currency we are interested in
               selectInput(inputId = "currency2", label = "Currency you are interested? ",
                           selected = "BTC", choices = all_coins ),
               #Currency we want to compare with
               selectInput(inputId = "comparison2", label = "Currency you want to compare? ",
                           selected = "USD", choices = all_coins ),
               #Grouping by what timeframe in news counter
               selectInput(inputId = "grouping", label = "News count group by what timeframe",
                           selected = "3 hours", choices = c("Day" = "day", "12 Hours" = "12 hours", "6 Hours" = "6 hours", "3 Hours" = "3 hours", "1 Hour" = "hour") )
                        )
                      ,
               #End of Sidebar Panel-----------------
               
        #Main Panel: Plot
        mainPanel(
          plotlyOutput("lastweekPlot")
                 )
                #End of siderbarLayout
                    )
             
             
             
             
             
             
             
             
             ),#-------End of 2nd tab
    #------------------------------------3nd tabPanel-------------------------------------
    tabPanel("Price Comparison"
             ,
             sidebarLayout(
               #Sidebar Layout-----------------
               sidebarPanel(
                 #The id of the cryptocurrency
                 textInput( "cryptoID", label = "Enter Cryptocurrency ID", value = "BTC" )
               )
               ,
               #End of Sidebar Panel-----------------

               #Main Panel: Average price on six platform and Plot
               mainPanel(
                 htmlOutput("averageprice"),
                 DTOutput(outputId ="pricetable")
               )
               #End of siderbarLayout
             )#-------End of 3rd tab

             )
    
   ) #End NavBar
   
)#End of UI-------------------------




#----------------------------------------------------------------------------------
# Server of Shiny App
server <- function(input, output, session){
  
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
    slow_MA = 26,
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
    if( as.numeric(input$timerange[2] - input$timerange[1]) > 26 * 30 ){
            updateSelectInput(session, inputId = "timeframe", label = "Choose first and last day: ",
                        selected = "month", choices = c("Month" = "month", "Week" = "week", "Day" = "day") )
        }
        #If the timerange is bigger than 3 months but smaller than a year, timeframe can be week or day
        else if( as.numeric(input$timerange[2] - input$timerange[1]) > 26 * 7 ){
          updateSelectInput(session, inputId = "timeframe", label = "Choose first and last day: ",
                        selected = "week", choices = c( "Week" = "week", "Day" ="day") )
        }
        #If the timerange is bigger than 1 month but smaller than 3, timeframe can only be week or day
        else if( as.numeric(input$timerange[2] - input$timerange[1]) > 26 ){
          updateSelectInput(session, inputId = "timeframe", label = "Choose first and last day: ",
                        selected = "day", choices = c( "Day" = "day") )
        }
        #If the timerange is smaller than 1 week, timeframe can be day or hour
        else {
          updateSelectInput(session, inputId = "timeframe", label = "Choose first and last day: ",
                        selected = "hour", choices = c("Hour" = "hour") )
        }
  })

  #------------------------------------------------------------------------------------
  
  #First tab plot: Currency prices, moving averages,...; default MACD
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
    
    
    #Print the latest price
    output$latestprice <- renderText({
      autoInvalidate()
      glue::glue("<font color=\"#FF0000\"><TT><font size=10>{getLastPriceBitfinex(coin, compare)}</TT></font>     <p><TT><font size=3>update time: {as.character(Sys.time())}</TT></font>")
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
    param2$cryptocurrency <- input$currency2
    param2$comparison <- input$comparison2
    param2$grouping <- input$grouping
  })
  
  #------------------------------------------------------------------------------------
  
  #Second tab plot: Currency prices from last week per minute; News count
  output$lastweekPlot <- renderPlotly({
    
    
    lastweekplot <- plot_lastweek(cryptocurrency = param2$cryptocurrency, comparison = param2$comparison,
                                  grouping = param2$grouping)
    
    lastweekplot
  })
  

  #-------------------third tab---------------------------------------------------------------
  #All parameters for 3rd tab
  param3 <- reactiveValues(
    cryptoID = "BTC"
  )

  observe({
    param3$cryptoID <- input$cryptoID
  })

  #------------------------------------------------------------------------------------

  #table: compare different price on different online platforms
  output$pricetable <- renderDT({ 
    pricecomparison <- getLastPriceMultiplePlatforms(param3$cryptoID)
    pricecomparison
    })
  output$averageprice <- renderText({
    pricecomparison <- getLastPriceMultiplePlatforms(param3$cryptoID)
    meanprice <- round(mean(as.numeric(pricecomparison$Price), na.rm = TRUE), digits = 2)
    glue::glue("<font color=\"#FF0000\"><TT><font size=3>The average price is</TT></font> <TT><font size=10>{meanprice}</TT></font> <TT><font size=3>{param3$cryptoID}/USD</TT></font>   <p><TT><font size=3>update time: {as.character(Sys.time())}</TT></font>")
  })
  
  
  
} #End of server---------

# Run the application 
shinyApp(ui = ui, server = server)

