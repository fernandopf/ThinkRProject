usethis::use_build_ignore("devstuff.R")
options(usethis.full_name= "Jiahao Wang")
usethis::use_mit_license()

#vignette
usethis::use_vignette("userguide")
utils::globalVariables(c("."), add = FALSE)
# Dependencies
usethis::use_package("dplyr")
usethis::use_package("jsonlite")
usethis::use_package("lubridate")
usethis::use_package("pracma")
usethis::use_pipe()
usethis::use_package("curl")
usethis::use_package("utils")
#Documents
devtools::document()
