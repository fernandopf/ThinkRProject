# Cryptocurrency Project!

### Introduction

This is a project for the subject "R for Data Science" of the MSc Data Science for Business, taught jointly by Hec Paris and École Polytechnique.

It consisted on the analysis of the cryptocurrency market:

* First, we receiver the data from the API Cryptocompare and we perform an analysis from different cryptocurrencies.
* Second, we analyse the impact the news have on the cryptocurrencies price.
* Third, we compare the price of the cryptocurrencies on the 6 biggest platform.
* Finally, we have implemented a trading simulator.

### Prerequisites

Before installing the package, you need to install the dependencies by running the file "dependencies.R"

It can be found on the folder "inst" inside the package or in the following link:

https://github.com/fernandopf/ThinkRProject/blob/master/CryptoShiny/inst/dependencies.R

The packages with the dependencies can be installed directly.
```
install.packages("devtools") # if you have not installed "devtools" package
install("<directory of the package>", dependencies = TRUE)

```

### Installing

After installing the dependecies and downloading the packages, you can install it by running the following command.

```
install.package(<directory of the package>")

```

### Loading the package

After installing the package you need to load it.

```
library(CryptoShiny)

```

### Shiny App

For running the Shiny App you need to call the function "run_app()"
```
run_app()
```

### Vignette

You can understand and learn more about the project by reading the Vignette.

```
 browseVignettes(package = "CryptoShiny")
```

### Functions to receive the data

The user can use several functions to get the data of the cryptocurrencies from the API.

The function "day_hour.R" has been designed to receive the data of the desired cryptocurrency in an specific timeframe. It returns a dataframe with the date, high, low, close and open price in the timeframe, volume in the and direction (decreasing if the close price is lower than the open price, increasing otherwise).

```
# Example to obtain the price of the Bitcoin in USD, from December 1th 2017 until August 1th 2018 with a daily timeframe.

bitcoinVsDollarExampleDay <- day_hour("day", "01/12/2017", "01/08/2018", "BTC", "USD")
```

The function "crypto.R" returns a dataset with the time, highest price, lowest price, open price, close price and financial indicators in the chosen timeframe.

```
# Example of the Bitcoin price vs USD per hour.

exampleCryptoBTCUSDHour <- crypto("hour", "01/08/2018", "01/10/2018", "BTC", "USD",5 , 26, 12, 9)
```

The function "getLastPriceMultiplePlatform.R" returns a dataset with the price of that cryptocurrency in each platform (if avaliable).

```
# Example to get the BTC price vs USD

lastPrice <- getLastPriceMultiplePlatforms("BTC")
```

The "function lastweek_minute.R" returns a dataframe with the date, high price, low price, close price, open price and direction (decreasing if the close price is lower than the open price, increasing otherwise) in each minute of the last week. 

```
# In the following example we are getting the price of Bitcoin in USD

minuteExampleBTCvsUSD <- lastweek_minute("BTC", "USD")

```

### Functions to plot the data

The function "candle_plot.R" returns an interactive plot displaying the evolution of the exchange rate between two currencies over time.

```
# Example
candle_plot(data= exampleCryptoBTCUSDHour, MACD)
```

The function "plot_lastweek.R" returns a plot where the evolution of the exchange rate between two currencies is displayed for a pre-specified time interval.

```
# Example to obtain the price of the Bitcoin in USD, from December 1th 2017 until August 1th 2018 with a daily timeframe.

plot_lastweek(cryptocurrency = "EOS", comparison = "GBP", grouping = "6 hours")
```

### Functions to analyse the data

The function "crypto_correlation.R" can be used to get the correlation of two cryptocurrencies between two chosen dates.

```
# Example to get the correlation of Bitcoin and Ethereum between 01/09/2018 and 01/10/2018.

correlationBTCvsETH <- crypto_correlation("01/09/2018", "01/10/2018", "BTC", "ETH")
```

The function "lastweek_news_counter.R" has been designed to count hourly how many times the inputted crpytocurrency has been mentioned in the news during the last week.

```
# Example to get how many times the Bitcoin has been mentioned during the last week

countNewsLastWeekBitcoin <- lastweek_news_counter("BTC")
```

The function "averages.R" has been designed to add financial indicators into the dataset (Moving average and MACD). It takes as input a dataset and the windows of moving average, slow MACD, quick MACD and signal MACD and returns the dataset with the financial indicators added.

```
# Example to add financial indicators to the dataset bitcoinVsDollarExampleWeek

bitcoinVsDollaFinancialIndicators <- averages(bitcoinVsDollarExampleWeek, 5, 26, 12, 9)
```

### Functions to analyse and retrieve the news data


The cryptocompare API gives us access to data about all the news articles that were released about cryptocurrencies from 2013 to now. Our goal is to interpret this news to see the evolution of the popularity of different cryptocurrencies during time.

For this, we want to analyse how many time any cryptocurrency was mentioned in the news, each hour and each day.

First we will try to download all the articles, their text body and the time each one was released. This first dataframe is huge and we will only keep the analysed ones.

### Getting the whole CryptoNewsData set 

This dataset stores all the news articles about cryptocurrencies released from 2013-08-01 18:30:00 that CryptoCompare API proposes

Here we download the dataset and store it to a csv file with dl_data_news function
```{r}
#data <- dl_data_from(as.numeric(as.POSIXct("2013-11-25 1:00:00 EST")))
#write.csv(data, "CryptonewsData.csv")
```

### Analysing the dataset for plots data

We load the dataset from csv, as the dl_data_from function would take too much time, so we run it only once.

Here the time is modelised by the timestamp, which is a very useful and can be converted into a date and precise hour: https://www.unixtimestamp.com/

```{r}
head(CryptonewsData)
```

We want to analyse the occurences of the 20 most capitalized cryptocurrencies; so we download the list of these cryptocurrencies symbols using get_imp_Crp function.

Here we removed some the 20 currencies that were not available in our crypto dictionary.

```{r}
#interesting_crypto <- get_imp_Crp()
#interesting_crypto
```

This vector of cryptocurrencies symbol will be translated into the name of each cryptocurrency by the function CryptoCurrencyName.
This function uses the data from a "Radiovisual" github: "https://github.com/crypti/cryptocurrencies/blob/master/cryptocurrencies.json)"

```{r}
CryptoCurrencyName("BTC")
CryptoCurrencyName("ETH")
```

By using CryptoCurrencyName dictionary, the function analyse_crps_news will verify if the each cryptocurrency was mentioned in those news articles.

We analyse the news dataset based on the cryptocurencies we chose: we have a dataframe showing which articles mentions each cryptocurrency.

```{r}
# interesting_crypto <- c("BTC", "ETH", "LTC", "XMR", "USDT")
# result <- analyse_crps_news(CryptonewsData[seq(1,500,1),], interesting_crypto)
# head(result)
```

Finally; we sum the occurences of each cryptocurrency per hour and per day for plotting; using the functions resTspToHour and resTspToDay.

We then write them in csvs file to create the two datasets CryptoNewsOccurencesHour and CryptoNewsOccurencesDay

```{r}
# finalHour <- resTspToHour(result, interesting_crypto)
# write.csv(finalHour, "CryptoNewsOccurencesHour.csv")
# head(finalHour)
# finalDay <- resTspToDay(result, interesting_crypto)
# write.csv(finalDay, "CryptoNewsOccurencesDays.csv")
# head(finalDay)
```

To make sure these datasets stay up to date for live usage, we can update them with the following functions: This updates the raw data in the folder raw-data.

```{r}
# updateHourNewsData()
```

```{r}
# updateDayNewsData()
```




## Authors

* Ching-Yu LIN
* Konstantinos PETROPOULOS
* Fernando PEREZ FERNANDEZ
* Jiahao WANG
* Souhail ELAISSAOUI

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

