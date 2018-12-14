#' crypto_correlation
#'
#' Function to calculate the correlation between two crypto currencies
#'
#' This function is designed to calcualte the correlation between two cryptocurrencies in a given timeframe
#'
#' @param firstDay first day to analyse in dd/mm/yyyy format
#' @param lastDay last day to analyse in dd/mm/yyyy format
#' @param cryptoA first cryptocurrency to correlate
#' @param cryptoB second cryptocurrency to correlate
#'
#' @return correlation between crypto A and crypto B
#' @importFrom dplyr select mutate
#' @importFrom stats cor
#'
#' @examples
#' \dontrun{
#' crypto_correlation("01/09/2018", "01/10/2018", "BTC", "ETH")
#'}
#' @export
crypto_correlation <- function(firstDay, lastDay, cryptoA, cryptoB) {
  cryptoAData <- day_hour("hour", firstDay, lastDay, cryptoA) %>%
    mutate(avg = (high + low) / 2) %>%
    select(avg)
  cryptoBData <- day_hour("hour", firstDay, lastDay, cryptoB) %>%
    mutate(avg = (high + low) / 2) %>%
    select(avg)
  return(cor(cryptoAData, cryptoBData)[1])
}
