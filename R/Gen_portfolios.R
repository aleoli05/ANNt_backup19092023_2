#' Gen_portfolios
#' @export
#' @param N_Assets limit of asset numbers in the portfolio
#' @param Initial_Date_Testing Initial Date of Test Series
#' @param Final_Date_Testing Final Date Test Series
#' @param Rf Risk free rate
#'
#' @examples
#' N_Assets <- 5
#' Initial_Date_Testing <- c('2023-01-03')
#' Final_Date_Testing <- c('2023-09-07')
#' Rf <- 0
#'
#' # Generate assets portfolio (maximum N assets specified)
#' Gen_portfolios(5,'2023-01-03','',0)
#'
Gen_portfolios <-function(N_Assets, Initial_Date_Testing, Final_Date_Testing, Rf){

  # Duração do processamento 1720/length(dados)=1.2 min)
  load("~/scenario.set.rda") # Carrega objeto scenario.set
  load("~/T8.rda") # Carrega objeto scenario.set
  load("~/I_dataPredict.rda") # Carrega objeto scenario.set
  load("~/F_dataPredict.rda") # Carrega objeto scenario.set

  dados<-scenario.set
  nAtivos = ncol(dados)

  Fator_Tempo = 1808/(nrow(dados))
  Unidade=' minute(s)'
  Tempo= round(Fator_Tempo*(ncol((dados))-1),2)
  if (Tempo>120){
    Unidade=' hour(s)'
    Tempo=round(Tempo/60,2)
  }
  dados2=data.frame(dados)
  cat(paste("
           Estimated total processing time: ", Tempo, Unidade,"
___________________________________________________________________
", sep=""))

  n_assets=N_Assets

  if(Initial_Date_Testing==('')){
    Initial_Date_Testing=I_dataPredict
  }
  if(Final_Date_Testing==('')){
    Final_Date_Testing=rownames(dados2[nrow(dados2),])
    #Final_Date_Testing=Sys.Date()
  }

  Rf=Rf/100

  scenario.set = data.frame(scenario.set)
  Datas1Predict = rownames(scenario.set)[
    (which(rownames(scenario.set)==Initial_Date_Testing)):(nrow(scenario.set)-1)]

  PosCovidSP500 = as.matrix(scenario.set[Datas1Predict,1])
  colnames(PosCovidSP500)=colnames(scenario.set[1])
  rownames(PosCovidSP500)=Datas1Predict
  TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,-1]))

  library(quantmod)
  library(PortfolioAnalytics)
  library(PerformanceAnalytics)
  #library(nse2r)
  library(MFDFA)
  library(xts)
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
  options(warn=-1)

  a<-0.9
  N<-1024
  tsx<-MFsim(N,a)

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Test
  #### Particular por Probabilidade t de Student
  ResProb_MFractal = matrix(ncol=nAtivos-1,nrow=1)
  ResProb_MFractal = data.frame(ResProb_MFractal)
  colnames(ResProb_MFractal)=colnames(scenario.set)[-1]
  ResProb_MFractal

  # Criando as vari?veis como vetor para treinamento - com datas espec?ficas
  for (k in 1:(nAtivos-1)){
    ativo = k+1
    #Envelope

    # Calculo das defasagens para cada ativo
    dat_MF <- data.frame(dados[,ativo])

    Inicio_data = Initial_Date_Testing
    Fim_data = Final_Date_Testing
    I_data = which(rownames(dat_MF)==Inicio_data)
    F_data = which(rownames(dat_MF)==Fim_data)
    entradas = as.matrix(dat_MF[I_data:F_data,])
    saidas = as.matrix(dat_MF[(I_data+1):(F_data+1),1])

    scale=10:100
    q<--10:10
    m<-1
    b_MF<-MFDFA(as.timeSeries(dat_MF), scale, m, q)

    # MDM - Market Deficiency Measure
    MDM = 1/2*(abs((b_MF$Hq[1]-0.5))+abs(b_MF$Hq[11]-0.5))
    ## Not run:
    ## Results plot ####


    # Resultados das Probabilidades

    ResProb_MFractal[1,k]= MDM

  }

  # Classificacao Muti_Fractal
  #Classificacao_MFractal <- sort(ResProb_MFractal, decreasing=FALSE)
  nomes = colnames(ResProb_MFractal)
  prob = t(ResProb_MFractal)
  Res=as.matrix(ResProb_MFractal)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  Classificacao_MFractal= matrix(data=ncol(ResProb_MFractal):1, nrow=1, ncol=ncol(ResProb_MFractal))
  colnames(Classificacao_MFractal)= Test$Nomes
  Classificacao_MFractal[1,]=Test$Prob

  #Classificacao_MFractal <- sort(ResProb_MFractal, decreasing=TRUE)

  # Carteira Multifractal
  Class=colnames(Classificacao_MFractal)
  Carteira_MFractal = Class[1:n_assets]
  C_MFractal = as.data.frame(scenario.set) %>%
    dplyr::select(which((colnames(scenario.set) %in% Carteira_MFractal)))
  C_MFractal = C_MFractal[Datas1Predict,]
  Pesos_MFractal = c(rep(1/n_assets,n_assets))
  Ret_C_MFractal = as.matrix(C_MFractal) %*% Pesos_MFractal

  # Carteira de Markovitz de Minima Variância M_Fractal
  Pesos_MFractal_Mkv <- round(tseries::portfolio.optim(as.matrix(C_MFractal))$pw, 4)
  Ret_C_MFractal = as.matrix(C_MFractal)%*% Pesos_MFractal_Mkv

  # Weight extract
  C_Pesos_MFractal_Mkv = data.frame(Carteira_MFractal,Pesos_MFractal_Mkv)
  Pesos_MFractal_Mkv1 <- C_Pesos_MFractal_Mkv[C_Pesos_MFractal_Mkv[,2]>0,]
  Pesos_MFractal_Mkv2<- t(matrix(Pesos_MFractal_Mkv1[,2]))
  colnames(Pesos_MFractal_Mkv2) <- Pesos_MFractal_Mkv1[,1]
  rownames(Pesos_MFractal_Mkv2)<-'Weight'
  #print(paste('Weights of the MF-MKW Portfolio:'))
  #   print(Pesos_MFractal_Mkv2)
  ################################################################################
  # Comparativo com a Carteira Multi Fractal _ MF
  # Carteira de Buffet - 7 ativos - 80%
  scenario.set = data.frame(scenario.set)

  #Datas1Predict = rownames(scenario.set)[
  # (which(rownames(scenario.set)=="2020-01-20")):(nrow(scenario.set)-1)]

  # Carteira de Markovitz de Minima Variância obtida a partir de todos ativos
  TodosAtivosPredict = as.matrix(rbind(scenario.set[Datas1Predict,-1]))
  pesos_todosPredict <- round(tseries::portfolio.optim(TodosAtivosPredict)$pw, 4)
  RetornoMedioMArkovitz = TodosAtivosPredict%*% pesos_todosPredict
  # Weight extract
  Carteira_Markov = data.frame(colnames(TodosAtivosPredict),pesos_todosPredict)
  Pesos_C_Markov <- Carteira_Markov[Carteira_Markov[,2]>0,]
  Pesos_C_Markov2 <- t(matrix(Pesos_C_Markov[,2]))
  colnames(Pesos_C_Markov2) <- Pesos_C_Markov[,1]
  rownames(Pesos_C_Markov2)<-'Weight'
  print(paste('[1] weights of the MARKOWITZ Portfolio:'))
  print(Pesos_C_Markov2)

  print(paste('[2] Weights of the MF_MKW Portfolio:'))
  print(Pesos_MFractal_Mkv2)

  CarteiraComparativa = colnames(T8[1:n_assets])
  C_Net_T_comparativa = as.data.frame(scenario.set) %>%
    dplyr::select(which((colnames(scenario.set) %in% CarteiraComparativa)))
  C_Net_T_comparativa = C_Net_T_comparativa[Datas1Predict,]

  # Carteira de pesos iguais
  PesosComparativos = c(rep(1/n_assets,n_assets))
  Media_C_Net_T_Comparativa = as.matrix(C_Net_T_comparativa) %*% PesosComparativos

  # Weight extract
  C_Pesos_Eq_ANNt= data.frame(colnames(C_Net_T_comparativa),PesosComparativos)
  Pesos_ANNt_Eq1 <- C_Pesos_Eq_ANNt[C_Pesos_Eq_ANNt[,2]>0,]
  Pesos_ANNt_Eq2<- round(t(matrix(Pesos_ANNt_Eq1[,2])),4)
  colnames(Pesos_ANNt_Eq2) <- Pesos_ANNt_Eq1[,1]
  rownames(Pesos_ANNt_Eq2)<-'Weight'
  print(paste('[3] Weights of the ANNt_EQ Portfolio:'))
  print(Pesos_ANNt_Eq2)


  # Carteira RNA NNet dist T com pesos de Markovitz  para Comparação
  pesos_MarkovitzNNet_T <- round(tseries::portfolio.optim(
    as.matrix(C_Net_T_comparativa))$pw, 4)
  Ret_Medio_RNA_T_Mkv = as.matrix(C_Net_T_comparativa) %*% pesos_MarkovitzNNet_T

  # Weight extract
  C_Pesos_MKV_ANNt= data.frame(colnames(C_Net_T_comparativa),pesos_MarkovitzNNet_T)
  Pesos_ANNt_Mkv1 <- C_Pesos_MKV_ANNt[C_Pesos_MKV_ANNt[,2]>0,]
  Pesos_ANNt_Mkv2<- t(matrix(Pesos_ANNt_Mkv1[,2]))
  colnames(Pesos_ANNt_Mkv2) <- Pesos_ANNt_Mkv1[,1]
  rownames(Pesos_ANNt_Mkv2)<-'Weight'
  print(paste('[4] Weights of the ANNt_MKW Portfolio:'))
  print(Pesos_ANNt_Mkv2)

  ################################cARTEIRAS SHARPE ###############################
  ### Carteira Sharpe todos os ativos
  ## Optmization
  symbols = colnames(TodosAtivosPredict)
  #####

  #init.portf <- portfolio.spec(assets = symbols)
  #init.portf <- add.constraint(portfolio = init.portf, type = "full_investment")
  #init.portf <- add.constraint(portfolio = init.portf, type = "long_only")
  #init.portf <- add.objective(portfolio = init.portf, type = "return", name = "mean")
  #init.portf

  #init.portf <- add.constraint(portfolio = init.portf, type = "risk",
  #                             name = "StdDev", multiplier = 0)
  #port1 <- add.constraint(portfolio = init.portf,
  #                        type = "diversification", min=0, max=1,
  #                        indexnum=2)
  #port1 <- add.constraint(portfolio = init.portf, type = "risk", name = "StdDev")


  ### Carteira Sharpe todos os ativos
  #maxSRport.rp <- optimize.portfolio(R=TodosAtivosPredict,
  #                                  portfolio = port1,
  #                                   optimize_method = "random",
  #                                  search_size = 20000,
  #                                 maxSR=TRUE, trace = TRUE)
  #maxSRport.rp

  #maxSR.weight.rp <- extractWeights(maxSRport.rp)


  all.returns <- TodosAtivosPredict
  ## set up portfolio with objetive and constraints
  n.assets <- length(colnames(all.returns))
  port.sec <- portfolio.spec(assets = colnames(all.returns))
  port.sec <- add.objective(portfolio = port.sec, type = "risk", name = "StdDev")
  port.sec <- add.objective(portfolio = port.sec, type = "return", name = "mean")
  port.sec <- add.constraint(portfolio = port.sec, type = "full_investiment")
  port.sec <- add.constraint(portfolio = port.sec, type = "box", min = 0, max = 1)

  # map off efficient frontier (for variance risk)
  eff.frontier <- create.EfficientFrontier(R = all.returns, portfolio = port.sec,
                                           n.portfolio = 2000, type = "mean-StdDev")

  # Daily Sharpe ratio
  rf=(1+Rf)^(1/252)-1
  sharpe.ratios <- (eff.frontier$frontier[,"mean"]-rf)/eff.frontier$frontier[,"StdDev"]
  max.sharpe.ratio <- sharpe.ratios[sharpe.ratios==max(sharpe.ratios)]
  optimal.port.name <- names(max.sharpe.ratio)
  optimal.mean <- eff.frontier$frontier[optimal.port.name,"mean"]
  optimal.sd <- eff.frontier$frontier[optimal.port.name,"StdDev"]

  n.trading.days.per.year <- 1

  #print(sprintf("Optimal Sharpe Ratio: %f", max.sharpe.ratio*sqrt(n.trading.days.per.year)))
  #print(sprintf("Optimal E(port return): %f", optimal.mean*sqrt(n.trading.days.per.year)))
  mean_sharpe = optimal.mean*sqrt(n.trading.days.per.year)
  #print(sprintf("Optimal sd(port return): %f", optimal.sd*sqrt(n.trading.days.per.year)))
  sd_sharpe <- optimal.sd*sqrt(n.trading.days.per.year)

  #print("Optimal weights")
  weight_test <- eff.frontier$frontier[optimal.port.name,(1:n.assets)+3]
  weight_test <- round(weight_test,4)
  weight_Sharpe= weight_test[which(weight_test !=0)]
  weight_Sharpe

  # Weight extract
  Weight_Sharpe_1 <- t(as.data.frame(weight_Sharpe))
  colnames(Weight_Sharpe_1)<-str_replace(colnames(Weight_Sharpe_1),'w.','')
  rownames(Weight_Sharpe_1)<-'Weight'

  print(paste('[5] Weights of the SHARPE Portfolio:'))
  print(Weight_Sharpe_1)
  #weight
  ### Retornos carteira Sharpe todos os ativos
  #RetornoMedioMaxIS = as.matrix(TodosAtivosPredict)%*% maxSR.weight.rp
  RetornoMedioMaxIS = as.matrix(TodosAtivosPredict)%*% weight_test

  ### Carteira Sharpe MF_DFA
  ## Optmization
  #symbols_MFractal = colnames(C_MFractal)
  #init.portf.MF <- portfolio.spec(assets = symbols_MFractal)
  #init.portf.MF <- add.constraint(portfolio = init.portf.MF, type = "full_investment")
  #init.portf.MF <- add.constraint(portfolio = init.portf.MF, type = "long_only")
  #init.portf.MF <- add.objective(portfolio = init.portf.MF, type = "return", name = "mean")
  #init.portf.MF

  #init.portf.MF <- add.constraint(portfolio = init.portf.MF, type = "risk",
  #                               name = "StdDev", multiplier = 0)
  #port1.MF <- add.constraint(portfolio = init.portf.MF,
  #                         type = "diversification", min=0, max=1,
  #                        indexnum=2)
  #port1.MF <- add.constraint(portfolio = init.portf.MF, type = "risk", name = "StdDev")


  #maxSRport.rp.MFractal <- optimize.portfolio(R=C_MFractal,
  #                                           portfolio = port1.MF,
  #                                          optimize_method = "random",
  #                                         search_size = 2000,
  #                                        maxSR=TRUE, trace = TRUE)
  #maxSRport.rp.MFractal

  #maxSR.weight.rp.MFractal <- extractWeights(maxSRport.rp.MFractal)
  #maxSR.weight.rp.MFractal

  #RetornoMedioMaxIS_MFractal = as.matrix(C_MFractal)%*% maxSR.weight.rp.MFractal

  ##################################################################
  all.returns.MF <- as.matrix(C_MFractal)
  ## set up portfolio with objetive and constraints
  n.assets.MF <- length(colnames(all.returns.MF))
  port.sec.MF <- portfolio.spec(assets = colnames(all.returns.MF))
  port.sec.MF <- add.objective(portfolio = port.sec.MF, type = "risk", name = "StdDev")
  port.sec.MF <- add.objective(portfolio = port.sec.MF, type = "return", name = "mean")
  port.sec.MF <- add.constraint(portfolio = port.sec.MF, type = "full_investiment")
  port.sec.MF <- add.constraint(portfolio = port.sec.MF, type = "box", min = 0, max = 1)

  # map off efficient frontier (for variance risk)
  eff.frontier.MF <- create.EfficientFrontier(R = all.returns.MF, portfolio = port.sec.MF,
                                              n.portfolio = 2000, type = "mean-StdDev")

  # Daily Sharpe ratio
  rf=(1+Rf)^(1/252)-1
  sharpe.ratios.MF <- (eff.frontier.MF$frontier[,"mean"]-rf)/eff.frontier.MF$frontier[,"StdDev"]
  max.sharpe.ratio.MF <- sharpe.ratios.MF[sharpe.ratios.MF==max(sharpe.ratios.MF)]
  optimal.port.name.MF <- names(max.sharpe.ratio.MF)
  optimal.mean.MF <- eff.frontier.MF$frontier[optimal.port.name.MF,"mean"]
  optimal.sd.MF <- eff.frontier.MF$frontier[optimal.port.name.MF,"StdDev"]

  n.trading.days.per.year.MF <- 1

  #print(sprintf("Optimal Sharpe Ratio: %f", max.sharpe.ratio.MF*sqrt(n.trading.days.per.year.MF)))
  #print(sprintf("Optimal E(port return): %f", optimal.mean.MF*sqrt(n.trading.days.per.year.MF)))
  mean_sharpe.MF = optimal.mean.MF*sqrt(n.trading.days.per.year.MF)
  #print(sprintf("Optimal sd(port return): %f", optimal.sd.MF*sqrt(n.trading.days.per.year.MF)))
  sd_sharpe.MF <- optimal.sd.MF*sqrt(n.trading.days.per.year.MF)

  #print("Optimal weights")
  weight_test_MF <- eff.frontier.MF$frontier[optimal.port.name.MF,(1:n.assets.MF)+3]
  weight_test_MF <- round(weight_test_MF,4)
  weight_Sharpe_MF= weight_test_MF[which(weight_test_MF !=0)]

  # Weight extract
  Weight_Sharpe_MF <- t(as.data.frame(weight_Sharpe_MF))
  colnames(Weight_Sharpe_MF)<-str_replace(colnames(Weight_Sharpe_MF),'w.','')
  rownames(Weight_Sharpe_MF)<-'Weight'

  print(paste('[6] Weights of the MF_SHARPE Portfolio:'))
  print(Weight_Sharpe_MF)

  ### Retornos carteira Sharpe MF_DFA Multifractal
  RetornoMedioMaxIS_MFractal = as.matrix(C_MFractal)%*% weight_test_MF


  ### Carteira Sharpe RNAt
  ## Optmization
  #symbols_RNAt = colnames(C_Net_T_comparativa)

  #init.portf <- portfolio.spec(assets = symbols_RNAt)
  #init.portf <- add.constraint(portfolio = init.portf, type = "full_investment")
  #init.portf <- add.constraint(portfolio = init.portf, type = "long_only")
  #init.portf <- add.objective(portfolio = init.portf, type = "return", name = "mean")
  #init.portf

  #init.portf <- add.constraint(portfolio = init.portf, type = "risk",
  #                            name = "StdDev", multiplier = 0)
  #port1 <- add.constraint(portfolio = init.portf,
  #                       type = "diversification", min=0, max=1,
  #                      indexnum=2)
  #port1 <- add.constraint(portfolio = init.portf, type = "risk", name = "StdDev")



  #maxSRport.rp.RNAt<- optimize.portfolio(R=C_Net_T_comparativa,
  #                                      portfolio = port1,
  #                                     optimize_method = "random",
  #                                    search_size = 2000,
  #                                   maxSR=TRUE, trace = TRUE)
  #maxSRport.rp.RNAt

  #maxSR.weight.rp.RNAt <- extractWeights(maxSRport.rp.RNAt)

  #RetornoMedioMaxIS_RNAt = as.matrix(C_Net_T_comparativa)%*% maxSR.weight.rp.RNAt

  ################################################################################
  ### Retornos carteira Sharpe RNAt
  all.returns_RNA_t <- as.matrix(C_Net_T_comparativa)
  ## set up portfolio with objetive and constraints
  n.assets.RNAt <- length(colnames(all.returns_RNA_t))

  port.sec.RNAt <- portfolio.spec(assets = colnames(all.returns_RNA_t))
  port.sec.RNAt <- add.objective(portfolio = port.sec.RNAt, type = "risk", name = "StdDev")
  port.sec.RNAt <- add.objective(portfolio = port.sec.RNAt, type = "return", name = "mean")
  port.sec.RNAt <- add.constraint(portfolio = port.sec.RNAt, type = "full_investiment")
  port.sec.RNAt <- add.constraint(portfolio = port.sec.RNAt, type = "box", min = 0, max = 1)

  # map off efficient frontier (for variance risk)
  eff.frontier_RNA_t <- create.EfficientFrontier(R = all.returns_RNA_t, portfolio = port.sec.RNAt,
                                                 n.portfolio = 2000, type = "mean-StdDev")

  # Daily Sharpe ratio
  rf=(1+Rf)^(1/252)-1
  sharpe.ratios_RNA_t <- (eff.frontier_RNA_t$frontier[,"mean"]-rf)/eff.frontier_RNA_t$frontier[,"StdDev"]
  max.sharpe.ratio_RNA_t <- sharpe.ratios_RNA_t[sharpe.ratios_RNA_t==max(sharpe.ratios_RNA_t)]
  optimal.port.name.RNAt <- names(max.sharpe.ratio_RNA_t)
  optimal.mean.RNAt <- eff.frontier_RNA_t$frontier[optimal.port.name.RNAt,"mean"]
  optimal.sd.RNAt <- eff.frontier_RNA_t$frontier[optimal.port.name.RNAt,"StdDev"]

  n.trading.days.per.year.RNAt <- 1

  #print(sprintf("Optimal Sharpe Ratio: %f", max.sharpe.ratio_RNA_t*sqrt(n.trading.days.per.year.RNAt)))
  #print(sprintf("Optimal E(port return): %f", optimal.mean.RNAt*sqrt(n.trading.days.per.year.RNAt)))
  mean_sharpe_RNA_t = optimal.mean.RNAt*sqrt(n.trading.days.per.year.RNAt)
  #print(sprintf("Optimal sd(port return): %f", optimal.sd.RNAt*sqrt(n.trading.days.per.year.RNAt)))
  sd_sharpe_RNA_t <- optimal.sd.RNAt*sqrt(n.trading.days.per.year.RNAt)
  #print("Optimal weights")
  weight_test_RNAt <- eff.frontier_RNA_t$frontier[optimal.port.name.RNAt,(1:n.assets.RNAt)+3]
  weight_test_RNAt <- round(weight_test_RNAt,4)
  weight_Sharpe_RNA_t= weight_test_RNAt[which(weight_test_RNAt !=0)]
  weight_Sharpe_RNA_t

  # Weight extract
  Weight_ANNt_Sharpe <- t(as.data.frame(weight_Sharpe_RNA_t))
  colnames(Weight_ANNt_Sharpe)<-str_replace(colnames(Weight_ANNt_Sharpe),'w.','')
  rownames(Weight_ANNt_Sharpe)<-'Weight'
  print(paste('[7] Weights of the ANNt_SHARPE Portfolio:'))
  print(Weight_ANNt_Sharpe)

  RetornoMedioMaxIS_RNAt = as.matrix(C_Net_T_comparativa)%*% weight_test_RNAt


  #####
  #############################################################################

  # Geração da Matriz de comparação dos Retornos
  Comparativo_RETORNOS = matrix(nrow=length(Ret_C_MFractal), ncol=5)
  Comparativo_RETORNOS[,1] = Ret_C_MFractal
  Comparativo_RETORNOS[,2] = PosCovidSP500
  Comparativo_RETORNOS[,3] = Media_C_Net_T_Comparativa
  Comparativo_RETORNOS[,4] = RetornoMedioMArkovitz
  Comparativo_RETORNOS[,5] = Ret_Medio_RNA_T_Mkv
  #Comparativo_RETORNOS[,6] = RetornoMedioMean_Variance_Mkv
  colnames(Comparativo_RETORNOS)= c("MF-DFA_Mkw","SP500", "RNAt_Eq", "Markowitz", "RNAt_Mkw")
  rownames(Comparativo_RETORNOS) = rownames(PosCovidSP500)
  Datas_Comparativo_RETORNOS = rownames(as.data.frame(Comparativo_RETORNOS))
  Comparativos_RETORNOS_Df = mutate(as.data.frame(Datas_Comparativo_RETORNOS),
                                    as.data.frame(Comparativo_RETORNOS))
  View(Comparativo_RETORNOS)
  save(Comparativo_RETORNOS,file='~/Comparativos_RETORNOS.rda')
  write_xlsx(Comparativos_RETORNOS_Df, "~/RetornosRNAxMF_DFA.xlsx")

  options(warn=-1)
  #
  # Geração da Matriz de comparação dos Retornos Acumulados
  Comparativo = matrix(nrow=length(Ret_C_MFractal), ncol=8)
  Comparativo[1,1] = PosCovidSP500[1,]
  Comparativo[1,2] = RetornoMedioMArkovitz[1,]
  Comparativo[1,3] = RetornoMedioMaxIS[1,]
  Comparativo[1,4] = Ret_C_MFractal[1,]
  Comparativo[1,5] = RetornoMedioMaxIS_MFractal[1,]
  Comparativo[1,6] = Media_C_Net_T_Comparativa[1,]
  Comparativo[1,7] = Ret_Medio_RNA_T_Mkv [1,]
  Comparativo[1,8] = RetornoMedioMaxIS_RNAt[1,]


  for(i in 2:length(PosCovidSP500)) {
    Comparativo[i,1] = (Comparativo[i-1,1]+1)*(PosCovidSP500[i,]+1)-1
    Comparativo[i,2] = (as.matrix(Comparativo[i-1,2])+1)*
      (as.matrix(RetornoMedioMArkovitz[i,])+1)-1
    Comparativo[i,3] = (as.matrix(Comparativo[i-1,3])+1)*
      (as.matrix(RetornoMedioMaxIS[i,])+1)-1
    Comparativo[i,4] = (as.matrix(Comparativo[i-1,4])+1)*
      (as.matrix(Ret_C_MFractal[i,])+1)-1
    Comparativo[i,5] = (as.matrix(Comparativo[i-1,5])+1)*
      (as.matrix(RetornoMedioMaxIS_MFractal[i,])+1)-1
    Comparativo[i,6] = (as.matrix(Comparativo[i-1,6])+1)*
      (as.matrix(Media_C_Net_T_Comparativa[i,])+1)-1
    Comparativo[i,7] = (as.matrix(Comparativo[i-1,7])+1)*
      (as.matrix(Ret_Medio_RNA_T_Mkv[i,])+1)-1
    Comparativo[i,8] = (as.matrix(Comparativo[i-1,8])+1)*
      (as.matrix(RetornoMedioMaxIS_RNAt[i,])+1)-1
  }
  RM <- colnames(scenario.set[1])
  colnames(Comparativo)= c(RM,"MARKOWITZ", "SHARPE", "MF_MKW", "MF_SHARPE",
                           "ANNt_EQ", "ANNt_MKW", "ANNt_SHARPE")
  rownames(Comparativo) = rownames(as.data.frame(PosCovidSP500))

  save(Comparativo,file='~/Comparativo.rda')
  write_xlsx(as.data.frame(Comparativo), "~/Comparativo_MF_DFA_Sharpe.xlsx")

  #### Matrix of weights
  Weights_All <- matrix(ncol=60, nrow=22)
  Weights_All <- as.data.frame((Weights_All))
  Weights_All [1,1] <- 'PORTFOLIOS'
  Weights_All [1,2] <- 'ASSETS'
  Weights_All [2,1] <- 'MARKOWITZ'
  for(k in (1:ncol(Pesos_C_Markov2))){
    Weights_All[2,k+1]=data.frame(colnames(Pesos_C_Markov2))[k,]
    Weights_All[3,k+1]=round(data.frame(Pesos_C_Markov2)[k],2)
  }
  Weights_All [4,1] <- 'MF_MKW'
  for(k in (1:ncol(Pesos_MFractal_Mkv2))){
    Weights_All[4,k+1]=data.frame(colnames(Pesos_MFractal_Mkv2))[k,]
    Weights_All[5,k+1]=round(data.frame(Pesos_MFractal_Mkv2)[k],2)
  }
  Weights_All [6,1] <- 'ANNt_EQ'
  for(k in (1:ncol(Pesos_ANNt_Eq2))){
    Weights_All[6,k+1]=data.frame(colnames(Pesos_ANNt_Eq2))[k,]
    Weights_All[7,k+1]=round(data.frame(Pesos_ANNt_Eq2)[k],2)
  }
  Weights_All [8,1] <- 'ANNt_EQ'
  for(k in (1:ncol(Pesos_ANNt_Mkv2))){
    Weights_All[8,k+1]=data.frame(colnames(Pesos_ANNt_Mkv2))[k,]
    Weights_All[9,k+1]=round(data.frame(Pesos_ANNt_Mkv2)[k],2)
  }
  Weights_All [10,1] <- 'SHARPE'
  for(k in (1:ncol(Weight_Sharpe_1))){
    Weights_All[10,k+1]=data.frame(colnames(Weight_Sharpe_1))[k,]
    Weights_All[11,k+1]=round(data.frame(Weight_Sharpe_1)[k],2)
  }
  Weights_All [12,1] <- 'MF_SHARPE'
  for(k in (1:ncol(Weight_Sharpe_MF))){
    Weights_All[12,k+1]=data.frame(colnames(Weight_Sharpe_MF))[k,]
    Weights_All[13,k+1]=round(data.frame(Weight_Sharpe_MF)[k],2)
  }
  Weights_All [14,1] <- 'ANNt_SHARPE'
  for(k in (1:ncol(Weight_ANNt_Sharpe))){
    Weights_All[14,k+1]=data.frame(colnames(Weight_ANNt_Sharpe))[k,]
    Weights_All[15,k+1]=round(data.frame(Weight_ANNt_Sharpe)[k],2)
  }
  save(Weights_All,file='~/Weights_All.rda')
  write_xlsx(as.data.frame(Weights_All), "~/Weights_All.xlsx")

  View(Weights_All)
  View(Comparativo)
}
