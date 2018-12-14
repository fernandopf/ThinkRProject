#' NetUSDValue
#'
#' this function is used to calculate the net USD value in the pocket which is a vector contains different currencies you currently have
#'
#' @param pocket a vector contains the units of currencies you have
#' @param day the net value in this day
#' @export
#'
#' @return a number indicates the net value
#' @example
#' \dontrun{
#' pocket <- c("USD" = 1000, "BTC" = 10, "ETH" = 5)
#' NetUSDValue(pocket = pocket, day = "11/12/2018" )
#' }
NetUSDValue <- function(pocket, day = "11/12/2018") {
  netusdvalue <- 0
  for (name in names(pocket)) {
    #loop for none zero value currencies
    if (name != "date" & name != "NetUSDvalue") {
      if (pocket[[name]] != 0) {
        df <- day_hour("day", day, day, name, "USD")
        netusdvalue = netusdvalue + df[2, 4] * pocket[[name]]
      }
    } #end of if
  }#end of for
  return(netusdvalue)
}
