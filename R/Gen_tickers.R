#' Generate tickers
#' @export
#' @ param Tickers Name of the assets or "Current_SP500_Tickers" for all S&P 500 assets
#' @examples
#' # Specify the assets or "Current_SP500_Tickers" for all S&P 500 assets
#' # Apply the "load" command and use in "Asset_series" command
#' Generate_Tickers <-c('PETR4.SA','AAPL','Current_SP500_Tickers')
#'
#' # Generates the Adjusted Daily Prices Series from Yahoo Finance
#' Gen_tickers (Generate_Tickers=c('PETR4.SA','AAPL','Current_SP500_Tickers'))
#' load('Generate_Tickers.rda')


Gen_tickers=function(Generate_Tickers){


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

  ################################################################################

  ###############################
  #install.packages("rvest")


  library(rvest)
  Tickers=Generate_Tickers
  Tickers_1=Tickers
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
  Current_SP500<-sp500tickers$Symbol
  #######

  x=as.numeric(any(c('Current_SP500_Tickers') %in% Tickers_1))
  if (x==1) {
    y=which(Tickers_1 %in% c('Current_SP500_Tickers'))
    Tickers_2=Tickers[-y]

    z=as.numeric(any(Tickers_2 %in% Current_SP500))
    if (z==1) {
      h=which(Tickers_2 %in% Current_SP500)
      Tickers_3=Tickers_2[-h]
      Tick=c(Tickers_3,Current_SP500)
    }

    Tick=c(Tickers_2,Current_SP500)
  } else {
    Tick=Tickers_1

  }
Generate_Tickers=Tick

save(Generate_Tickers,file='~/Generate_Tickers.rda')
save(Generate_Tickers,file='Generate_Tickers.rda')
print(Generate_Tickers)
load('~/Generate_Tickers.rda')
}

################################################################################
