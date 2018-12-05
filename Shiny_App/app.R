
library(shiny)
library(shinythemes)
library(markdown)
library(CryptoProject)
library(plotly)


#Reading all possible currencies from CSV
name_coins <- read.csv("data/coins.csv", header = FALSE)[,1]
all_coins <- as.list(name_coins)
names(all_coins) <- name_coins

#User interface
ui <- fluidPage(
   theme = shinytheme("sandstone"),
   
   # Application title
   titlePanel("Cryptocurrency Project"),
  
   navbarPage("CrytoProject!",
    #-------------------------------1st tabPanel--------------------------------------
      tabPanel("Plot",
         
         fluidRow(
           
           #Currency selector
           column(3, wellPanel(
             selectInput(inputId = "currency", label = "Currency you are interested? ",
                         selected = "BTC", choices = all_coins )
                    )
           ),
           #Currency to compare with
           column(3, wellPanel(
             selectInput(inputId = "comparison", label = "Currency you want to compare? ",
                         selected = "USD", choices = all_coins )
                    )
           ),
           # Timerange selector
           column(3, wellPanel(
             dateRangeInput(inputId = "timerange", 
                            label = "Choose first and last day: ",
                            start = Sys.Date() - 90, end = Sys.Date(),
                            separator = " - ", format = "dd/mm/yy",
                            startview = 'year', language = 'fr', weekstart = 1)
                    )
          ),
          #Timeframe selector
           column(3, wellPanel(
             selectInput(inputId = "timeframe", label = "Choose first and last day: ",
                         selected = "day", choices = c("Day" = "day") )
                    )
           )
          ), #End of fluidRow
         
         fluidRow(
           #Moving average
           column(3, wellPanel(
             numericInput( "MA",
                           label = "Moving Average (MA)",
                           value = 5, min = 1, max = 85, step = 1
             )
           )
           ),
           #Supplementary plot choice
           column(3, wellPanel(
             actionButton(inputId = "MACD", label = "MACD"),
             actionButton(inputId = "VOLUME", "volume")
           )
           )
         ),
         mainPanel(
           plotlyOutput("mainPlot")
         )
      ),
    #------------------------------------2nd tabPanel-------------------------------------
    tabPanel("Other things"),
    navbarMenu("More", 
      tabPanel("Table"),
      tabPanel("About")
    )
   ) #End NavBar
)

#----------------------------------------------------------------------------------
# Server of Shiny App
server <- function(input, output, session){
  
  param <- reactiveValues(
    frame = "day",
    start = Sys.Date() - 90,
    end = Sys.Date() ,
    coin = "BTC",
    compare = "USD",
    ma = 5,
    quick_MACD = 12,
    slow_MACD = 26,
    signal_MACD = 9
  )
  
  observe({
    param$frame <- input$timeframe
    param$timerange <- input$timerange
    param$coin <- input$currency
    param$compare <- input$comparison
    param$ma <- input$MA
  })
  
  observe({
    if( as.numeric(input$timerange[2] - input$timerange[1]) > 24 * 30 ){
            updateSelectInput(session, inputId = "timeframe", label = "Choose first and last day: ",
                        selected = "month", choices = c("Month" = "month", "Week" = "week", "Day" = "day") )
        }
        #If the timerange is bigger than 3 months but smaller than a year, timeframe can be month, week or day
        else if( as.numeric(input$timerange[2] - input$timerange[1]) > 24 * 7 ){
          updateSelectInput(session, inputId = "timeframe", label = "Choose first and last day: ",
                        selected = "week", choices = c( "Week" = "week", "Day" ="day") )
        }
        #If the timerange is bigger than 1 month but smaller than 3, timeframe can only be week or day
        else if( as.numeric(input$timerange[2] - input$timerange[1]) > 24 ){
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
    
    df <- crypto(timeframe = frame, firstDay = format(start, "%d/%m/%Y"), 
                 lastDay = format(end, "%d/%m/%Y"), 
                 cryptocurrency = coin, comparison = compare, 
                 n_MA = ma, n_quick_MACD = 12, n_slow_MACD = 26, n_signal_MACD = 9)
    
    candle_plot(df, MACD)
  })
  
  #Change main plot if user choose MACD
  observeEvent(input$MACD, {
    output$mainPlot <- renderPlotly({
      
      frame <- param$frame
      start <- param$timerange[1]
      end <- param$timerange[2]
      coin <- param$coin
      compare <- param$compare
      ma <- param$ma

      df <- crypto(timeframe = frame, firstDay = format(start, "%d/%m/%Y"),
                  lastDay = format(end, "%d/%m/%Y"),
                  cryptocurrency = coin, comparison = compare,
                  n_MA = ma, n_quick_MACD = 12, n_slow_MACD = 26, n_signal_MACD = 9)
      candle_plot(df, MACD)
    })
  })
  
  #Change main plot if user choose VOLUME
  observeEvent(input$VOLUME, {
    output$mainPlot <- renderPlotly({
      
      frame <- param$frame
      start <- param$timerange[1]
      end <- param$timerange[2]
      coin <- param$coin
      compare <- param$compare
      ma <- param$ma

      df <- crypto(timeframe = frame, firstDay = format(start, "%d/%m/%Y"),
                   lastDay = format(end, "%d/%m/%Y"),
                   cryptocurrency = coin, comparison = compare,
                   n_MA = ma, n_quick_MACD = 12, n_slow_MACD = 26, n_signal_MACD = 9)
      candle_plot(df, volume)
    })
  })
  



}

# Run the application 
shinyApp(ui = ui, server = server)

