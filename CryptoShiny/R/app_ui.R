#' app_ui
#'
#' @import shiny
#' @importFrom shinydashboard tabBox
#' @importFrom shinythemes shinytheme
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#' @importFrom shinycssloaders withSpinner
#'
#'
#'
app_ui <- function() {

  #Reading all possible currencies from CSV
  name_coins <- (CryptoShiny::coins)
    #read.csv("data/coins.csv", header = FALSE)[,1]
  all_coins <- as.list(name_coins)
  no_currency_coins <- as.list(name_coins[-c(1,2,3),])  #Without USD, EUR and GBP
  #Global parameter
  n <- 24
  options(spinner.color ="#A9A9A9")


    fluidPage(
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
                                           min = "2010-01-01", max = Sys.Date(),
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
                              tabBox( id = "tabset", selected = "MACD", side = "left", width = 10,
                                      tabPanel(title = "MACD", value = "MACD",

                                               #Number of points used to compute the slow moving average
                                               numericInput( inputId= "slow_MA",
                                                             label = "Slow EMA",
                                                             value = n, min = 2, max = 85, step = 1),

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
                            ),
                            actionButton(inputId = "Run_tab1", label= "RUN APP")

                          ), #End of Sidebar Panel-----------------

                          #Main Panel: Plot
                          mainPanel(
                            htmlOutput("latestprice"),
                            withSpinner(plotlyOutput("mainPlot"))
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
                                        selected = "3 hours",
                                        choices = c("Day" = "day", "12 Hours" = "12 hours",
                                                    "6 Hours" = "6 hours", "3 Hours" = "3 hours", "1 Hour" = "hour")),
                            actionButton(inputId = "Run_tab2", label= "RUN APP")
                          ),
                          #End of Sidebar Panel-----------------

                          #Main Panel: Plot
                          mainPanel(
                            withSpinner(plotlyOutput("lastweekPlot"))
                          )
                          #End of siderbarLayout
                        )


               ),#-------End of 2nd tab

               #------------------------------------3rd tabPanel-------------------------------------
               tabPanel("Price Comparison"
                        ,
                        sidebarLayout(
                          #Sidebar Layout-----------------
                          sidebarPanel(
                            #The id of the cryptocurrency
                            #textInput( "cryptoID", label = "Enter Cryptocurrency ID", value = "BTC" )
                            selectInput(inputId = "cryptoID", label = "Enter Cryptocurrency ID",
                                        selected = "BTC", choices = no_currency_coins ),
                            actionButton(inputId = "Run_tab3", label= "RUN APP")
                          ),
                          #End of Sidebar Panel-----------------

                          #Main Panel: Average price on six platform and Plot
                          mainPanel(
                            htmlOutput("averageprice"),
                            withSpinner(DTOutput(outputId ="pricetable"))
                          )
                          #End of siderbarLayout
                        )#-------End of 3rd tab

               )

    ) #End NavBar

  )#End of UI-------------------------
}
