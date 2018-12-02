
library(shiny)
library(shinythemes)
library(markdown)
library(CryptoProject)

#User interfqce
ui <- fluidPage(
   theme = shinytheme("sandstone"),
   
   # Application title
   titlePanel("Cryptocurrency Project"),
  
   navbarPage("CrytoProject!",
    #-------------------------------1st tabPanel--------------------------------------
      tabPanel("Plot",
         # Timeframe selector
         fluidRow(
           column(3, wellPanel(
             dateRangeInput(inputId = "timerange", 
                            label = "Choose first and last day: ",
                            start = Sys.Date() - 5, end = Sys.Date() + 5,
                            separator = " - ", format = "dd/mm/yy",
                            startview = 'year', language = 'fr', weekstart = 1)
            )
          ),
           column(3, wellPanel(
             selectInput(inputId = "timeframe", label = "Choose first and last day: ",
                         selected = "day", choices = c("Day" = "day") )
            )
           ),
           column(3, wellPanel(
             selectInput(inputId = "currency", label = "Currency you are interested? ",
                         selected = "BTC", choices = c("BTC" = "BTC") )
            )
           ),
           column(3, wellPanel(
             selectInput(inputId = "comparison", label = "Currency you want to compare? ",
                         selected = "USD", choices = c("USD" = "USD") )
            )
           ),
          mainPanel(
            plotOutput("mainPlot")
          )
          ) #End of fluidRow
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
server <- function(input, output, session) {
  
  # observe({
  #   if (!is.null(input$timerange) ){
  #     if( as.numeric(input$timerange[2] - input$timerange[1]) > 12 * 30 ){
  #             updateSelectInput(session, inputId = "timeframe", label = "Choose first and last day: ",
  #                         selected = "month", choices = c("Month" = "month", "Week" = "week") )
  #         }
  #         #If the timerange is bigger than 3 months but smaller than a year, timeframe can be month, week or day
  #         else if( as.numeric(input$timerange[2] - input$timerange[1]) > 3*30 ){
  #           updateSelectInput(session, inputId = "timeframe", label = "Choose first and last day: ",
  #                         selected = "week", choices = c("Month" = "month", "Week" = "week", "Day" ="day") )
  #         }
  #         #If the timerange is bigger than 1 month but smaller than 3, timeframe can only be week or day
  #         else if( as.numeric(input$timerange[2] - input$timerange[1]) > 30 ){
  #           updateSelectInput(session, inputId = "timeframe", label = "Choose first and last day: ",
  #                         selected = "day", choices = c("Week" = "week", "Day" = "day") )
  #         }
  #         #If the timerange is bigger 1 week but smaller than a month, timeframe can only be day
  #         else if( as.numeric(input$timerange[2] - input$timerange[1]) > 7 ) {
  #           updateSelectInput(session, inputId = "timeframe", label = "Choose first and last day: ",
  #                           selected = "day", choices = c("Day" = "day") )
  #         }
  #         #If the timerange is smaller than 1 week, timeframe can be day or hour
  #         else {
  #           updateSelectInput(session, inputId = "timeframe", label = "Choose first and last day: ",
  #                         selected = "day", choices = c("Day" = "day", "Hour" = "hour") )
  #         }
  #   }
  # })
  
  # output$ui <- renderUI({
  #   #If the timerange is bigger than a year, timeframe can only be month or week
  #   if( as.numeric(input$timerange[2] - input$timerange[1]) > 12 * 30 ){
  #       selectInput(inputId = "timeframe", label = "Choose first and last day: ",
  #                   selected = "month", choices = c("Month" = "month", "Week" = "week") )
  #   }
  #   #If the timerange is bigger than 3 months but smaller than a year, timeframe can be month, week or day
  #   else if( as.numeric(input$timerange[2] - input$timerange[1]) > 3*30 ){
  #       selectInput(inputId = "timeframe", label = "Choose first and last day: ",
  #                   selected = "week", choices = c("Month" = "month", "Week" = "week", "Day" ="day") )
  #   }
  #   #If the timerange is bigger than 1 month but smaller than 3, timeframe can only be week or day
  #   else if( as.numeric(input$timerange[2] - input$timerange[1]) > 30 ){
  #       selectInput(inputId = "timeframe", label = "Choose first and last day: ",
  #                   selected = "day", choices = c("Week" = "week", "Day" = "day") )
  #   }
  #   #If the timerange is bigger 1 week but smaller than a month, timeframe can only be day
  #   else if( as.numeric(input$timerange[2] - input$timerange[1]) > 7 ) {
  #         selectInput(inputId = "timeframe", label = "Choose first and last day: ",
  #                     selected = "day", choices = c("Day" = "day") )
  #   }
  #   #If the timerange is smaller than 1 week, timeframe can be day or hour
  #   else {
  #       selectInput(inputId = "timeframe", label = "Choose first and last day: ",
  #                   selected = "day", choices = c("Day" = "day", "Hour" = "hour") )
  #   }
  # })

  #------------------------------------------------------------------------------------

  # param2 <- reactive({
  #   req(input$timerange, input$timeframe, input$currency, input$comparison)
  #   
  #   param$frame  <- input$timeframe
  #   param$start <-  input$timerange[1]
  #   param$end <-  input$timerange[2]
  #   param$coin <-  input$currency
  #   param$compare <-  input$comparison
  # })
  
  # r <- reactive({
  #   list(
  #     input$timeframe,
  #     input$timerange,
  #     input$currency,
  #     input$comparison
  #   )
  # })
  
  param <- reactiveValues(
    frame = "day",
    start = Sys.Date() - 5,
    end = Sys.Date() + 5,
    coin = "BTC",
    compare = "USD"
  )
  
  observe({

    param$frame <- input$timeframe
    param$timerange <- input$timerange
    param$coin <- input$currency
    param$compare <- input$comparison
  })
  
  output$mainPlot <- renderPlot({
    # frame <- r()[[1]]
    # start <- r()[[2]][1]
    # end <- r()[[2]][2]
    # coin <- r()[[3]]
    # compare <- r()[[4]]
    frame <- param$frame
    start <- param$timerange[1]
    end <- param$timerange[2]
    coin <- param$coin
    compare <- param$compare
    
      df <- crypto(timeframe = frame, firstDay = format(timerange[1], "%d/%m/%Y"), 
                   lastDay = format(timerange[2], "%d/%m/%Y"), 
                   crytocurrenty = coin, comparison = compare, 
                   n_MA = 5, n_quick_MACD = 12, n_slow_MACD = 26, n_signal_MACD = 9)

      candle_plot(df, MACD)
    })


  



}

# Run the application 
shinyApp(ui = ui, server = server)

