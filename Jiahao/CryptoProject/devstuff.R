usethis::use_build_ignore("devstuff.R")
options(usethis.full_name= "Jiahao Wang")
usethis::use_mit_license()

#vignette
usethis::use_vignette("userguide")

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
#Documents
devtools::document()
