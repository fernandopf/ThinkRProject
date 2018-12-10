
usethis::use_build_ignore("devstuff.R")
options(usethis.full_name= "Jiahao Wang")
usethis::use_mit_license()

#vignette
usethis::use_vignette("userguide")

#Attachments
attachment::create_dependencies_file()

# Dependencies
usethis::use_package("dplyr")
usethis::use_package("jsonlite")
usethis::use_package("lubridate")
usethis::use_package("pracma")
usethis::use_pipe()
usethis::use_package("curl")
usethis::use_package("utils")
usethis::use_package("plotly")
usethis::use_package("rlang")
usethis::use_package("stats")
usethis::use_package("httr")
usethis::use_package("ggplot2")
usethis::use_package("shinycssloaders")
usethis::use_package("shiny")
usethis::use_package("DT")
usethis::use_package("glue")
usethis::use_package("shinythemes")
usethis::use_package("shinydashboard")




#Documents
devtools::document()

coins <- read_csv("data-raw/coins.csv")
usethis::use_data(coins)

