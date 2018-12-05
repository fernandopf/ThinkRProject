
library(shiny)
library(shinythemes)
library(markdown)
library(CryptoProject)
library(plotly)
library(shinydashboard) #Make tabs as buttons

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
      tabPanel("Plot",
               
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
                                               label = "Slow Moving Average",
                                               value = 26, min = 2, max = 85, step = 1),
                                 
                                 #Number of points used to compute quick moving average
                                 numericInput( inputId= "quick_MA",
                                               label = "Quick Moving Average",
                                               value = 12, min = 2, max = 85, step = 1),
                                 
                                 #Number of points used to compute signal moving average
                                 numericInput( inputId= "signal_MA",
                                               label = "Signal Moving Average",
                                               value = 9, min = 2, max = 85, step = 1)
                                 ),
                        
                          tabPanel(title = "VOLUME", value = "VOLUME")
                        
                      ) #End of tabBox
                    )

          ), #End of Sidebar Panel-----------------
          
          #Main Panel: Plot
          mainPanel(
           plotlyOutput("mainPlot")
         )
      ) #End of Sidebar Layout
      
    ), #End of first tab
    
    #------------------------------------2nd tabPanel-------------------------------------
    tabPanel("Other things"),
    navbarMenu("More", 
      tabPanel("Table"),
      tabPanel("About")
    )
   ) #End NavBar
   
)#End of UI-------------------------




#----------------------------------------------------------------------------------
# Server of Shiny App
server <- function(input, output, session){
  
  #All parameters for 1st tab
  param <- reactiveValues(
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
    param$coin <- input$currency
    param$compare <- input$comparison
    param$ma <- input$MA
    param$slow_MA <- input$slow_MA
    param$quick_MA <- input$quick_MA
    param$signal_MA <- input$signal_MA
  })
  
  #Boundary for maximum number of points to compute the moving average
  observe({
    param$timerange <- input$timerange
    param$frame <- input$timeframe
    
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
    
    frame <- param$frame
    start <- param$timerange[1]
    end <- param$timerange[2]
    coin <- param$coin
    compare <- param$compare
    ma <- param$ma
    slow_ma <- param$slow_MA 
    quick_ma <- param$quick_MA 
    signal_ma <- param$signal_MA
    
    df <- crypto(timeframe = frame, firstDay = format(start, "%d/%m/%Y"), 
                 lastDay = format(end, "%d/%m/%Y"), 
                 cryptocurrency = coin, comparison = compare, 
                 n_MA = ma, n_quick_MACD = quick_ma, n_slow_MACD = slow_ma, n_signal_MACD = signal_ma)
    
    candle_plot(df, MACD)
  })
  
#Transforming plot when MACD or VOLUME tab selected
  observe({
    frame <- param$frame
    start <- param$timerange[1]
    end <- param$timerange[2]
    coin <- param$coin
    compare <- param$compare
    ma <- param$ma
    slow_ma <- param$slow_MA 
    quick_ma <- param$quick_MA 
    signal_ma <- param$signal_MA
    
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
  }) #End of observer for plot transformation due to tab selection

  
  
  
  
} #End of server---------

# Run the application 
shinyApp(ui = ui, server = server)

