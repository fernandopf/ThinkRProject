# No Remotes ----
# Attachments ----
to_install <- c("curl", "dplyr", "jsonlite", "lubridate", "magrittr", "plotly", "pracma", "rlang", "stats", "utils")
  for (i in to_install) {
    message(paste("looking for ", i))
    if (!requireNamespace(i)) {
      message(paste("     installing", i))
      install.packages(i)
    }

  }
