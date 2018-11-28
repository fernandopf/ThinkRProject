
ui <- fluidPage(
  theme= "bootstrap_linera.css",
  
  titlePanel("change css??"),
  fluidRow(
    
    column(2, wellPanel(
      radioButtons("timeframe", "Time Parameter", choices = 
                     c("Minutes"="min", "Hours"="hour", "Days"="day", "Week"="week", "Months"="month"),
                   inline = FALSE)
      )
    )),
    
    column(3, wellPanel(
      uiOutput("ui")
    ))
)


#================================================================================#
#================================================================================#
#================================================================================#

server <- function(input, output) {
  
  output$ui <- renderUI({
    switch(input$timeframe,
           "min" = sliderInput("time_range", 
                               "Choose Date Range:", 
                               min = as.POSIXlt("2018-02-01 01:00"),
                               max = as.POSIXlt("2018-03-01 23:00"),
                               value = c(as.POSIXlt("2018-02-01 02:00"), as.POSIXlt("2018-02-05 02:00")),
                               timeFormat = "%a %H:%M", ticks = F, step= 1),
           "hour" = dateRangeInput("hour_range", "Date Range",
                                   start = Sys.Date() - 2, end = Sys.Date() + 2,
                                   separator = " - ", format = "dd/mm/yy"),
           "day" =  dateRangeInput("hour_range", "Date Range",
                                   start = Sys.Date() - 2, end = Sys.Date() + 2,
                                   separator = " - ", format = "dd/mm/yy"),
           "week" = dateRangeInput("hour_range", "Date Range",
                                   start = Sys.Date() - 2, end = Sys.Date() + 2,
                                   separator = " - ", format = "dd/mm/yy",
                                   startview = 'year', language = 'fr', weekstart = 1),
           "month" = dateRangeInput("hour_range", "Date Range",
                                    start = Sys.Date() - 2, end = Sys.Date() + 2,
                                    separator = " - ", format = "dd/mm/yy",
                                    startview = 'year', language = 'fr', weekstart = 1)
           )
  })
  
}

shinyApp(ui = ui, server = server)