crypto_correlation <- function(firstDay, lastDay, cryptoA, cryptoB){
    library(dplyr)
    cryptoAData <- crypto_dataset_day_hour("hour", firstDay, lastDay, cryptoA ) %>% 
      mutate(avg = (high + low)/2) %>% 
      select(avg)
    cryptoBData <- crypto_dataset_day_hour("hour", firstDay, lastDay, cryptoB ) %>% 
      mutate(avg = (high + low)/2) %>% 
      select(avg)
    return(cor(cryptoAData,cryptoBData))
}
