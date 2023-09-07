#' ANNt
#' Generate portfolios with classification by the probability of return exceeding a RM,
#' using artificial neural networks and t-distribution.
#' @export
#' @param Tickers Name of the assets or "Current_SP500_Tickers" for all S&P 500 assets
#' @param RM Proxy of the market
#' @param Inicial_Date Series start Date
# @param Date_Traning Series finish training date
#' @param Final_Date Series end Date
#' #If the generating series is smaller than the specified one, it means that one of the assets has
#' #a shorter length than the specified one

#' @examples
#' # Specify the assets or "Current_SP500_Tickers" for all S&P 500 assets
#' Tickers <-c('AAPL','GOOG','CCBG', 'XOM', 'TSLA')
#' RM <-c('^GSPC') #RM the S&P500
#' Initial_Date <-c('2018-01-03')
#' Final_Date <-c('2023-01-09')
#'
#' # Generates the Adjusted Daily Prices Series from Yahoo Finance
#' Assets_series (Tickers=c('AAPL','GOOG','CCBG','XOM','TSLA'),'^GSPC', '2018-01-03', '2023-01-09')
#'
Assets_series <- function(Tickers, RM, Initial_Date, Final_Date) {

  # library(webinar.cpom)
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
  portfolioPrices_Teste = portfolioPrices


  Datas_portfolio = rownames(as.data.frame(portfolioPrices))
  portfolioPrices_Df = mutate(as.data.frame(Datas_portfolio),
                              as.data.frame(portfolioPrices))


  portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
                                           function(x) all(!is.na(x))),]
  portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
                                           function(x) all(!0)),]
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
  portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))

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
  Assets_Returns=scenario.set
  write.zoo(scenario.set, file='scenario.set')
  View(Assets_Prices)
  View(Assets_Returns)
  #write_xlsx(tickers,file='~/tickers.xlsx')
  write_xlsx(portfolioPrices_Df, "~/Assets_Prices.xlsx")
  scenario.set2=data.frame(scenario.set)
  write_xlsx(scenario.set2, "~/Assets_Returns.xlsx")
  write_xlsx(sp500tickers, "~/Current_SP500_Tickers.xlsx")

  # Plot Charts
  cat("\n", paste0(names(scenario.set), "\n"))

  chart.Bar(scenario.set$SP500)
  charts.PerformanceSummary(scenario.set$SP500)

  #########################################
}
