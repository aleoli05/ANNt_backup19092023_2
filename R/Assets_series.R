#' ANNt
#' Generate portfolios with classification by the probability of return exceeding a RM,
#' using artificial neural networks and t-distribution.
#' @export
#' @param Tickers Name of the assets or "Current_SP500_Tickers" for all S&P 500 assets
#' @param RM Proxy of the market
#' @param Inicial_Date Series start Date, format ('Year-Month-Day')
#' @param Final_Date Series end Date ('Year-Month-Day')
#' Assets with values not observedin the series are excluded

#' @examples
#' # Specify the assets or "Current_SP500_Tickers" for all S&P 500 assets
#' Tickers <-c('AAPL','GOOG','CCBG', 'XOM', 'TSLA')
#' RM <-c('^GSPC') #RM the S&P500
#' Initial_Date <-c('2018-01-03')
#' Final_Date <-c('2023-09-07')
#'
#' # Generates the Adjusted Daily Prices Series from Yahoo Finance
#' Assets_series (Tickers=c('AAPL','GOOG','CCBG','XOM','TSLA'),'^GSPC', '2018-01-03', '2023-01-09')
#'
Assets_series <- function(Tickers, RM, Initial_Date, Final_Date) {

  # library(webinar.cpom)
  install.packages("quantmod")
  install.packages("PerformanceAnalytics")
  install.packages("magrittr")
  install.packages("fBasics")
  install.packages("tidyverse")
  install.packages("stringr")
  install.packages("dplyr")
  install.packages("neuralnet")
  install.packages("zoo")
  install.packages("forecast")
  install.packages("timetk")
  install.packages("moments")
  install.packages("data.table")
  install.packages("ggplot2")
  install.packages("rvest")
  install.packages("caret")
  install.packages("readxl")
  install.packages("writexl")
  install.packages("portfolio.optimization")
  install.packages("PortfolioAnalytics")
  install.packages("ROI")
  install.packages("fPortfolio")
  install.packages("timeSeries")
  install.packages("gridExtra")
  install.packages("cowplot")
  install.packages("portfolioBacktest")
  install.packages("CVXR")
  install.packages("MFDFA")
  install.packages("DEoptim")

  library(quantmod)
  library(PerformanceAnalytics)
  library(magrittr)
  library(fBasics)
  library(tidyverse)
  library(stringr)
  library(dplyr)
  library(neuralnet)
  library(zoo)
  library(forecast)
  library(timetk)
  library(moments)
  library(data.table)
  library(ggplot2)
  library(rvest)
  library(caret)
  library (readxl)
  library(writexl)
  library(portfolio.optimization)
  library(PortfolioAnalytics)
  library(ROI)
  library(fPortfolio)
  library(timeSeries)
  library(gridExtra)
  library(cowplot)
  library(portfolioBacktest)
  library(CVXR)
  library(MFDFA)
  library(DEoptim)


  ################# Create Returns Times Series ###########################
  # RM
  RM <- RM
  Current_SP500_Tickers<-NULL
################################################################################

  ###############################
  #install.packages("rvest")

  library(rvest)
  # get the URL for the wikipedia page with all SP500 symbols
  url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
  # use that URL to scrape the SP500 table using rvest
  tickers_ <- url %>%
    # read the HTML from the webpage
    read_html() %>%
    # one way to get table
    #html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
    # easier way to get table
    html_nodes(xpath = '//*[@id="constituents"]') %>%
    html_table()
  #create a vector of tickers
  sp500tickers <- tickers_[[1]]
  sp500tickers = sp500tickers %>% mutate(Symbol =
                                           case_when(Symbol == "BRK.B" ~ "BRK-B",
                                                     Symbol == "BF.B" ~ "BF-B",
                                                     TRUE ~ as.character(Symbol)))
  Current_SP500_Tickers<-sp500tickers$Symbol
  #######
  if (length(Tickers)==1){
  if (Tickers == c('Current_SP500_Tickers')) {
    Tickers = Current_SP500_Tickers
  }}
  ################################################################################

  #Calculate Returns: Daily
  tickers <- c(RM,Tickers)
  portfolioPrices <- NULL

  for (Ticker in tickers)
    portfolioPrices <- cbind(portfolioPrices,
                             getSymbols.yahoo(Ticker, from= Initial_Date,
                                              to= Final_Date,
                                              auto.assign=FALSE)[,6])

  # Salve SP500 in excel
  colnames(portfolioPrices) <- str_replace(tickers,".Close","")
  colnames(portfolioPrices) <- str_replace(tickers,".Adjusted","")
  portfolioPrices_Teste = portfolioPrices


  Datas_portfolio = rownames(as.data.frame(portfolioPrices))
  portfolioPrices_Df = mutate(as.data.frame(Datas_portfolio),
                              as.data.frame(portfolioPrices))


  df=t(portfolioPrices)
  df2=df[apply(df,1,function(x) all(!is.na(x))),]
  df3<-t(df2)


  portfolio_observed <- df3[apply(df3,1,
                                           function(x) all(!is.na(x))),]
  portfolio_observed <- df3[apply(df3,1,
                                           function(x) all(!0)),]

 # portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
 #                                             function(x) all(!is.na(x))),]
#  portfolioPrices<- portfolioPrices[apply(portfolioPrices,1,
  #                                           function(x) all(!0)),]
  # RM
  RM <- c("SP500")
  #Renames Columns

  tickers <- str_replace(tickers,".SA","")
  tickers <- str_replace(tickers,"-","")
  tickers <- str_replace(tickers,"=","")
  tickers <- str_replace(tickers,"^G","G")
  colnames(portfolioPrices) <- tickers
  colnames(portfolioPrices)[1] <- RM


  #View(portfolioPrices)

  # Calculate Returns: Daily RoC

    #portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
  portfolioReturns <- na.omit(ROC(portfolio_observed, type="discrete"))

  scenario.set <- portfolioReturns

  scenario.set <- scenario.set[apply(scenario.set,1,
                                     function(x) all(!0)),]
  #View(scenario.set)

  assets <- ncol(scenario.set)
  scenarios <- nrow(scenario.set)
  saveRDS(scenario.set,file='scenario.set')
  save(scenario.set,file='senario.set.rda')
  save(tickers,file='~/tickers.rda')
  save(scenario.set,file='~/senario.set.rda')
  save(scenario.set,file='~/Assets_Returns.rda')
  save(portfolioPrices,file='~/Assets_Prices.rda')
  Assets_Prices=portfolioPrices
  Asset_Prices_Observed=portfolio_observed
  Assets_Returns=scenario.set
  write.zoo(scenario.set, file='scenario.set')
  View(Assets_Prices)
  View(portfolio_observed)
  View(Assets_Returns)
  #write_xlsx(tickers,file='~/tickers.xlsx')
  write_xlsx(portfolioPrices_Df, "~/Assets_Prices.xlsx")
  write_xlsx(portfolio_observed, "~/Assets_Prices_Observed.xlsx")
  scenario.set2=data.frame(scenario.set)
  write_xlsx(scenario.set2, "~/Assets_Returns.xlsx")
  write_xlsx(sp500tickers, "~/Current_SP500_Tickers.xlsx")

  # Plot Charts
  cat("\n", paste0(names(scenario.set), "\n"))

  chart.Bar(scenario.set$SP500)
  charts.PerformanceSummary(scenario.set$SP500)

  #########################################
}
