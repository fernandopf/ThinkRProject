#' transaction
#'
#' this function is used to make a transaction
#'
#' this function takes the input pocket_log and record the transaction in the new row of the pocket_log
#'
#' @param pocket_log a data.frame which have the columns date and different currencies
#' @param unit how many unit of buycurrency you want to buy
#' @param buycurrency the currency you want to buy
#' @param sellcurrency the currency you used to buy the buycurrency
#' @param day the day you do the transaction
#'
#' @return a updated pocket_log
#' @export transaction
#'
#'
#' @examples
transaction <- function(pocket_log, unit = 1, buycurrency = "BTC", sellcurrency = "USD", day = "05/12/2018") {
  df <- day_hour("day", day, day, buycurrency, sellcurrency)
  exchange <- df[2,4]
  #record the transaction date
  pocket_log[nrow(pocket_log)+1, 1] <- as.POSIXct(day,format="%d/%m/%Y", origin = "1970-01-01",tz = "GMT")
  #copy the amount of currencies from previous log
  pocket_log[nrow(pocket_log), 2 : ncol(pocket_log)] <- pocket_log[nrow(pocket_log) - 1, 2 : ncol(pocket_log)]
  #sell the currency and record on the lastest row at df[2,4] price
  pocket_log[nrow(pocket_log),sellcurrency] <- pocket_log[nrow(pocket_log),sellcurrency] - exchange * unit
  #buy the currency
  pocket_log[nrow(pocket_log),buycurrency ] <- pocket_log[nrow(pocket_log),buycurrency] + unit

  return(pocket_log)
  # for a single pocket
  # pocket[[sellcurrency]] <- pocket[[sellcurrency]]- unit*df[2,4]
  # pocket[[buycurrency]] <- pocket[[buycurrency]] + unit
  # return(pocket)
}
