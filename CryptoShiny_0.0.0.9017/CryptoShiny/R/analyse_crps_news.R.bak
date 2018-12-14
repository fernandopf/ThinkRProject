#' Analyses a dataframe with news body for occurences of the each of the currencySymbols
#'
#' @param databodynews dataframe with news body and their timestamp
#' @param currencySymbols a vector of characters representing the cryptocurrencys Symbols to be analysed
#' @return Dataframe with a boolean column per cryptosymbol, representing if the cryptocurrency was cited in the news article
#' @export analyse_crps_news
#' @importFrom dplyr filter mutate
analyse_crps_news <- function(databodynews, currencySymbols) {
  resul <- analyse_crp_news(databodynews, currencySymbols[1])[-2] #we remove the article body
  for (i in 2:length(currencySymbols)){ # here the for loop only iterates on few cryptocurrency symbols
    resul <- cbind(resul, analyse_crp_news(databodynews, currencySymbols[i])[3])
    }
  resul$time <- as.numeric(resul$time)
  return(resul)
}
