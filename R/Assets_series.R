#' Assets series
#' Import the series with the length specified
#' @export
#' @param Tickers Name of the assets or "Current_SP500_Tickers" for all S&P 500 assets
#' @param RM Proxy of the market
#' @param Initial_Date Series start Date, format ('Year-Month-Day')
#'
#' Assets with values not observed in the series are excluded
#' @param Final_Date Series end Date ('Year-Month-Day'). If '' is the System Date
#' @param Periodicity should be one of “daily”, “weekly”, “monthly”, “hourly”, “1minutes”, “2minutes”, “5minutes”, “15minutes”, “30minutes”, “60minutes”, “90minutes”. (Intraday maximum 7 days)


#' @examples
#' # Specify the assets or "Current_SP500_Tickers" for all S&P 500 assets
#' Tickers <-c('AAPL','GOOG','CCBG', 'XOM', 'TSLA')
#' RM <-c('^GSPC') #RM the S&P500
#' Initial_Date <-c('2018-01-03')
#' Final_Date <-c('2023-09-07')
#' Periodicity <- c('daily')
#'
#' # Generates the Adjusted Daily Prices Series from Yahoo Finance
#' Assets_series (Tickers=c('AAPL','GOOG','CCBG','XOM','TSLA'),'^GSPC', '2018-01-03', '','daily')
#'
Assets_series <- function(Tickers, RM, Initial_Date, Final_Date, Periodicity) {

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

  if(Final_Date==('')){
    Final_Date=Sys.Date()
  }


  # RM

  RM <- RM
  Tickers_1=Tickers
  Condicao=Tickers
  ################################################################################

  ###############################
  #install.packages("rvest")

  library(rvest)
  options(warn=-1)
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


Tickers=Tick
#########################################################################################
if (x==1) {

  Tickers = Current_SP500
  #Calculate Returns: Daily
  tickers <- c(RM,Tickers)
  portfolioPrices <- NULL

  for (Ticker in tickers)
    portfolioPrices <- cbind(portfolioPrices,
                             getSymbols.yahoo(Ticker, from= Initial_Date,
                                              to= Final_Date,
                                              periodicity= Periodicity,
                                              auto.assign=FALSE)[,6])

  # Salve SP500 in excel


  colnames(portfolioPrices) <- str_replace(tickers,".Close","")
  colnames(portfolioPrices) <- str_replace(tickers,".Adjusted","")
  tickers <- str_replace(tickers,".SA","")
  tickers <- str_replace_all(tickers,"^","")
  tickers= as.data.frame(tickers)
  tickers= tickers %>% mutate(tickers =
                                              case_when(tickers == "^BVSP" ~ "IBOV",
                                                       tickers == "^GSPC" ~ "SP500",
                                                      TRUE ~ as.character(tickers)))
  tickers = as.character(tickers[,1])
  tickers <- str_replace(tickers,"-","")
  tickers <- str_replace(tickers,"=","")
  colnames(portfolioPrices) <- tickers

  portfolioPrices_Teste = portfolioPrices


  Datas_portfolio = rownames(as.data.frame(portfolioPrices))
  portfolioPrices_Df = mutate(as.data.frame(Datas_portfolio),
                              as.data.frame(portfolioPrices))

  #Renames Columns
  tickers <- colnames(portfolioPrices)

  #if (length(Condicao)==1){
  df=t(portfolioPrices)
  df2=df[apply(df,1,function(x) all(!is.na(x))),]
  df3<-t(df2)


  portfolio_observed <- df3[apply(df3,1,
                                 function(x) all(!is.na(x))),]
  portfolio_observed <- df3[apply(df3,1,
                                 function(x) all(!0)),]
  portfolio_observed1 = portfolio_observed

  if(length(Tick)>length(Current_SP500)){
    g=which(Tick %in% Current_SP500)
    tickers= Tick[-g]

    #Calculate Returns: Daily
    tickers2 <- tickers
    portfolioPrices <- NULL

    for (Ticker in tickers2)
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols.yahoo(Ticker, from= Initial_Date,
                                                to= Final_Date,
                                                periodicity= Periodicity,
                                                auto.assign=FALSE)[,6])

    # Salve SP500 in excel


    colnames(portfolioPrices) <- str_replace(tickers,".Close","")
    colnames(portfolioPrices) <- str_replace(tickers,".Adjusted","")
    tickers <- str_replace(tickers,".SA","")
    tickers <- str_replace_all(tickers,"^","")
    tickers= as.data.frame(tickers)
    tickers= tickers %>% mutate(tickers =
                                  case_when(tickers == "^BVSP" ~ "IBOV",
                                            tickers == "^GSPC" ~ "SP500",
                                            TRUE ~ as.character(tickers)))
    tickers = as.character(tickers[,1])
    tickers <- str_replace(tickers,"-","")
    tickers <- str_replace(tickers,"=","")
    colnames(portfolioPrices) <- tickers

    portfolioPrices_Teste = portfolioPrices

    #portfolioPrices=cbind(portfolioPrices,portfolio_observed1)

    Datas_portfolio = rownames(as.data.frame(portfolioPrices))
    portfolioPrices_Df = mutate(as.data.frame(Datas_portfolio),
                                as.data.frame(portfolioPrices))

    #Renames Columns
    tickers <- colnames(portfolioPrices)
    portfolioPrices_Teste = portfolioPrices


    Datas_portfolio = rownames(as.data.frame(portfolioPrices))
    portfolioPrices_Df = mutate(as.data.frame(Datas_portfolio),
                                as.data.frame(portfolioPrices))
    portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
                                             function(x) all(!is.na(x))),]
    portfolioPrices<- portfolioPrices[apply(portfolioPrices,1,
                                            function(x) all(!0)),]
    portfolio_observed2=portfolioPrices
    d1=as.data.frame(cbind("id"=rownames(portfolio_observed1),portfolio_observed1))
    d2=as.data.frame(cbind("id"=rownames(as.data.frame(portfolio_observed2)),as.data.frame(portfolio_observed2)))
    # d1= data.frame(portfolio_observed1)
    # d2= data.frame(portfolio_observed2)
    portfolio_observed=full_join(d1,d2, by=c("id"))
    #portfolio_observed=full_join(d2,d1, by=colnames(d2[1]))
    rownames(portfolio_observed)=portfolio_observed[,1]
    portfolio_observed=as.matrix(portfolio_observed[,-1])

    port= matrix(ncol=ncol(portfolio_observed), nrow=nrow(portfolio_observed))
    for (i in 1:ncol(portfolio_observed)) {
      for (k in 1:nrow(portfolio_observed)) {
      port[k,i]=matrix(as.numeric(portfolio_observed[k,i]))
      }
    }
    colnames(port)=colnames(portfolio_observed)
    rownames(port)=rownames(portfolio_observed)
    portfolio_observed=port
    #portfolio_observed=merge(as.data.frame(d1),as.data.frame(d2), all=T)
  }}
   if (x==0) {

     Tickers_n = Tickers_1
     #Calculate Returns: Daily
     tickers <- c(RM,Tickers_n)
     portfolioPrices <- NULL

     for (Ticker in tickers)
       portfolioPrices <- cbind(portfolioPrices,
                                getSymbols.yahoo(Ticker, from= Initial_Date,
                                                 to= Final_Date,
                                                 periodicity= Periodicity,
                                                 auto.assign=FALSE)[,6])

     # Salve SP500 in excel


     colnames(portfolioPrices) <- str_replace(tickers,".Close","")
     colnames(portfolioPrices) <- str_replace(tickers,".Adjusted","")
     tickers <- str_replace(tickers,".SA","")
     tickers <- str_replace_all(tickers,"^","")
     tickers= as.data.frame(tickers)
     tickers= tickers %>% mutate(tickers =
                                   case_when(tickers == "^BVSP" ~ "IBOV",
                                             tickers == "^GSPC" ~ "SP500",
                                             TRUE ~ as.character(tickers)))
     tickers = as.character(tickers[,1])
     tickers <- str_replace(tickers,"-","")
     tickers <- str_replace(tickers,"=","")
     colnames(portfolioPrices) <- tickers

     portfolioPrices_Teste = portfolioPrices


     Datas_portfolio = rownames(as.data.frame(portfolioPrices))
     portfolioPrices_Df = mutate(as.data.frame(Datas_portfolio),
                                 as.data.frame(portfolioPrices))
   portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
                                function(x) all(!is.na(x))),]
   portfolioPrices<- portfolioPrices[apply(portfolioPrices,1,
                                 function(x) all(!0)),]
   portfolio_observed=portfolioPrices
  }

  if(RM=="^GSPC"){
    RM <- c("SP500")

  } else {
    if(RM=='^BVSP'){
      RM <- c("IBOV")

  }}

  colnames(portfolioPrices)[1] <- RM
  colnames(portfolio_observed)[1] <- RM


  # Calculate Returns: Daily RoC

    #portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
  portfolioReturns <- as.matrix(na.omit(ROC(portfolio_observed), type="discrete"))
  #portfolioReturns <- as.matrix(na.omit(portfolio_observed, type="discrete"))

  scenario.set <- portfolioReturns

  scenario.set <- scenario.set[apply(scenario.set,1,
                                     function(x) all(!0)),]
  #View(scenario.set)

  assets <- ncol(scenario.set)
  scenarios <- nrow(scenario.set)
  saveRDS(scenario.set,file='scenario.set')
  save(tickers,file='~/tickers.rda')
  save(scenario.set,file='~/scenario.set.rda')
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
  portfolio_observed2=data.frame(portfolio_observed)
  write_xlsx(portfolio_observed2, "~/Assets_Prices_Observed.xlsx")
  scenario.set2=data.frame(rownames(as.data.frame(scenario.set)),as.data.frame(scenario.set))
  write_xlsx(scenario.set2, "~/Assets_Returns.xlsx")
  write_xlsx(sp500tickers, "~/Current_SP500_Tickers.xlsx")

  # Plot Charts
  cat("\n", paste0(names(scenario.set), "\n"))

  chart.Bar(scenario.set[,1])
  charts.PerformanceSummary(scenario.set[,1])

  #########################################
}
