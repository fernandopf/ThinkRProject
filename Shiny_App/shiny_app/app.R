
library(shiny)
library(shinythemes)

#User interfqce
ui <- fluidPage(
   theme = shinytheme("sandstone"),
   
   # Application title
   titlePanel("Cryptocurrency Project"),
  
   # Timeframe selector
   fluidRow(
     column(3, wellPanel(
       dateRangeInput(inputId = "timerange", 
                      label = "Choose first and last day: ",
                      start = Sys.Date() - 2, end = Sys.Date() + 2,
                      separator = " - ", format = "dd/mm/yy",
                      startview = 'year', language = 'fr', weekstart = 1)
      )),
     column(3, wellPanel(
       uiOutput("ui")
     ))
   )


)

#----------------------------------------------------------------------------------
# Server of Shiny App
server <- function(input, output, session) {
  
  #When timerange chosen, we update the choices for timeframe
  observeEvent(input$timerange,{
    #If the timerange is bigger than a year, timeframe can only be month or week
    if( as.numeric(input$timerange[2] - input$timerange[1]) > 12 * 30 ){
      output$ui <- renderUI({ 
        selectInput(inputId = "timeframe", label = "Choose first and last day: ",
          selected = "mon", choices = c("Month" = "mon", "Week" = "week") )
        })
    }
    #If the timerange is bigger than 3 months but smaller than a year, timeframe can be month, week or day
    else if( as.numeric(input$timerange[2] - input$timerange[1]) > 3*30 ){
      output$ui <- renderUI({ 
        selectInput(inputId = "timeframe", label = "Choose first and last day: ",
                    selected = "Week", choices = c("Month" = "mon", "Week" = "week", "Day" ="day") )
      })
    }
    #If the timerange is bigger than 1 month but smaller than 3, timeframe can only be week or day
    else if( as.numeric(input$timerange[2] - input$timerange[1]) > 30 ){
      output$ui <- renderUI({ 
        selectInput(inputId = "timeframe", label = "Choose first and last day: ",
                    selected = "day", choices = c("Week" = "week", "Day" = "day") )
      })
    }
    #If the timerange is bigger 1 week but smaller than a month, timeframe can only be day
    else if( as.numeric(input$timerange[2] - input$timerange[1]) > 7 ) {
      output$ui <- renderUI({ 
        selectInput(inputId = "timeframe", label = "Choose first and last day: ",
                    selected = "day", choices = c("Day" = "day") )
      })
    }
    #If the timerange is smaller than 1 week, timeframe can be day or hour
    else {
      output$ui <- renderUI({ 
        selectInput(inputId = "timeframe", label = "Choose first and last day: ",
                    selected = "day", choices = c("Day" = "day", "Hour" = "hr") )
      })
    }
  })




}

# Run the application 
shinyApp(ui = ui, server = server)

