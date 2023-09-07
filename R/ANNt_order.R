#' ANNt_order
#' , after import the assets, with the command Asset_series,
#' classify assets by the probability of return exceeding a RM,
#' using artificial neural networks and t-distribution
#' @export
#' @param Inicial_Date Series start Date (Must be 7 perios greater than the analyzed seriess)
# @param Date_Training Series finish training date
#' @param Final_Date_Training Series end Date
#' @examples
#' Initial_Date_Training <-c('2018-01-11')
#' Final_Date_Training <- c('2022-12-30')
#' Final_Date_Testing <-c('2023-01-09')
#' ANNt_order ('2018-01-11', '2022-12-30','2023-01-09')
#' # Estimated processing time 30 minutes per asset
#'
ANNt_order <- function(Initial_Date_Training, Final_Date_Training, Final_Date_Testing) {
  ## Convers?o das variaveis
  # Excesso do retorno em relacao ao RM


  load("~/senario.set.rda") # Carrega objeto scenario.set
  load("~/tickers.rda") # Carrega objeto scenario.set

 dados<-scenario.set
  ncoldados <- ncol(dados)
  nAtivos = ncol(dados)
  for (i in 2:ncoldados) {
    dados[,i] <- dados[,i] - dados[,1]
  }

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Train
  #### NeuralNet por Sinal
  ResProbPosNNet = matrix(ncol=nAtivos-1,nrow=1)
  ResProbPosNNet = data.frame(ResProbPosNNet)
  colnames(ResProbPosNNet)=tickers[-1]
  ResProbPosNNet

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Train
  #### NeuralNet por Probabilidade t de Student
  ResProbTPosNNet = matrix(ncol=nAtivos-1,nrow=1)
  ResProbTPosNNet = data.frame(ResProbTPosNNet)
  colnames(ResProbTPosNNet)=tickers[-1]
  ResProbTPosNNet

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Train
  #### Particular por Sinal
  ResProbPos = matrix(ncol=nAtivos-1,nrow=1)
  ResProbPos = data.frame(ResProbPos)
  colnames(ResProbPos)=tickers[-1]
  ResProbPos

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Train
  #### Particular por Probabilidade t de Student
  ResProbTPos = matrix(ncol=nAtivos-1,nrow=1)
  ResProbTPos = data.frame(ResProbTPos)
  colnames(ResProbTPos)=tickers[-1]
  ResProbTPos

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Test
  #### Particular por Sinal
  ResProbPosNNetPredict = matrix(ncol=nAtivos-1,nrow=1)
  ResProbPosNNetPredict = data.frame(ResProbPosNNetPredict)
  colnames(ResProbPosNNetPredict)=tickers[-1]
  ResProbPosNNetPredict

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Test
  #### Particular por Probabilidade t de Student
  ResProbTPosNNetPredict = matrix(ncol=nAtivos-1,nrow=1)
  ResProbTPosNNetPredict = data.frame(ResProbTPosNNetPredict)
  colnames(ResProbTPosNNetPredict)=tickers[-1]
  ResProbTPosNNetPredict

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Test
  #### Particular por Sinal
  ResProbPosPredict = matrix(ncol=nAtivos-1,nrow=1)
  ResProbPosPredict = data.frame(ResProbPosPredict)
  colnames(ResProbPosPredict)=tickers[-1]
  ResProbPosPredict

  #### Cria??o da vari?vel de armazenamento dos resultados das RNAs Test
  #### Particular por Probabilidade t de Student
  ResProbTPosPredict = matrix(ncol=nAtivos-1,nrow=1)
  ResProbTPosPredict = data.frame(ResProbTPosPredict)
  colnames(ResProbTPosPredict)=tickers[-1]
  ResProbTPosPredict

  ########################
  #### Cria??o da vari?vel de armazenamento dos resultados de assimetria e curtose
  #### Particular por Probabilidade t de Student
  Resultados_Assim_Curtose = matrix(ncol=nAtivos-1,nrow=4)
  Resultados_Assim_Curtose = data.frame(Resultados_Assim_Curtose)
  colnames(Resultados_Assim_Curtose)=tickers[-1]
  Resultados_Assim_Curtose


  ################################################################################
  #####################################Envelope###################################
  ################################################################################

  for (k in 1:(nAtivos-1)){
    ativo = k+1
    #Envelope

    # Calculo das defasagens para cada ativo
    dat_r <- data.frame(dados[,ativo], Lag(dados[,1],1))
    colnames(dat_r)[2]="RM"
    defasagem = 5
    for (i in 1:defasagem){
      dat_r[i+2] = Lag(dat_r[1],i)
    }
    ### removendo NAs
    #dat_r = na.fill(dat_r, "extend")
    dat_r = na.omit(dat_r)
    nlinhas = nrow(dat_r)
    View(dat_r)

    ####################### Amostra de Tratamento################################
    # Criando as vari?veis como vetor para treinamento - com datas espec?ficas
    Inicio_data = as.Date(Initial_Date_Training)
    Fim_data = as.Date(Final_Date_Training)

    I_data = which(rownames(dat_r)==Inicio_data)
    F_data = which(rownames(dat_r)==Fim_data)
    entradas = as.matrix(dat_r[I_data:F_data,])
    saidas = as.matrix(dat_r[(I_data+1):(F_data+1),1])


    #####
    nlinhas <- nrow(entradas)
    ncolunas <- ncol(entradas)

    #############################################################################
    ##Pacote neuralnet()

    library(neuralnet)
    library(zoo)
    library(forecast)
    library(timetk)

    epocas = 25000
    # Fun??o Sigmoide
    sigmoide = function(soma) {
      #return (1/ (1+exp(-soma)))
      #Fun??o Tangente Hiperb?lica
      #return (1-tanh(soma))
      return (tanh(soma))
    }
    colnames(entradas)[1]= "ATIVO"
    nn= neuralnet( ATIVO ~ RM + V3 + V4 + V5 + V6 + V7, data=entradas,
                   hidden = nlinhas, act.fct = "tanh", threshold = 0.01,
                   stepmax=epocas)
    # Plotagem da RNA
    if(nlinhas %% 2 == 0) {
      escondida = nlinhas
    } else {escondida =nlinhas+1}
    nnplot= neuralnet( ATIVO ~ RM + V3 + V4 + V5 + V6 + V7, data=entradas,
                       hidden = escondida, act.fct = "tanh", threshold = 0.01,
                       stepmax=epocas)

    if (ativo==2){
    colnames(entradas)[1]=colnames(dat_r[ativo-1])
    } else {
      colnames(entradas)[1]=colnames(dat_r[ativo])
    }
    View(entradas)

    arquivo = colnames(entradas)[1]
    #arquivo = paste(arquivo,"_N.pdf", sep="")
    #str_sub(arquivo, -1) <-"_RNA.pdf"
    #ggsave(arquivo,p1)
    #pdf(file=arquivo, height = 8, width = 9)
    #pdf(file=arquivo)
    # plot(nnplot)
    #dev.off()
    #p1 <- plot(nnplot)
    #p1
    #arquivo <- str_replace(arquivo,"_N","")
    arquivo = paste(arquivo,".pdf", sep="")
    pdf(file=arquivo)
    #dev.print(pdf, file = arquivo)
    #ARQUIVO <- str_replace(arquivo,".pdf",".png")
    #ggsave(ARQUIVO,p1)


    ## Previs?o
    prev = predict(nn, entradas)
    nome = colnames(entradas)[1]
    plot(as.vector(entradas[,1]), type="l", col= "red",
         main = paste("Retornos Amostra de Tratamento - Ativo", xnames = nome))
    legend("topright", legend = c("RNNet", nome), pch = 19,
           col = c("black", "red"))
    lines(prev)

    hist(prev, main = paste("Histograma Fase de Treinamento RNNet Ativo",
                            xnames= nome),
         xlab = paste("Retorno Excedente sobre", xnames = "RM"))
    mean(prev)

    Testes = compute(nn, entradas)
    Testes$net.result
    #png(ARQUIVO)
    #arquivo = colnames(entradas)[1]
    #arquivo = paste(arquivo,"HistTrain.pdf", sep="_")
    #ggsave(arquivo,p1)
    #pdf(file=arquivo, height = 8, width = 9)
    hist(Testes$net.result, main = paste("Histograma Previs?es RNA Ativo",
                                         xnames= nome), xlab = paste("Retorno Excedente sobre",
                                                                     xnames = "RM"))
    #dev.off()
    #arquivo <- str_replace(arquivo,"pdf","png")
    #png(filename=arquivo)
    hist(Testes$net.result, main = paste("Histograma Train RNA Ativo",
                                         xnames= nome), xlab = paste("Retorno Excedente sobre",
                                                                     xnames = "RM"))
    #dev.off()
    ## Avaliando a acur?cia (ME, RMSE, MAE, MPE e MAPE)
    dat_r2 = na.fill(entradas, "extend")
    prev2 = na.fill(entradas, "extend")
    accuracy(as.vector(prev2),entradas[1])

    sinal = NULL
    nlinhas = nrow(entradas)
    for (i in 1:nlinhas) {
      if(prev[i] > 0){
        sinal[i] = 1
      }
      else {
        sinal[i] = -1
      }
    }
    sinal
    ### Probabilidades
    RetornoPos <- sum(sinal ==1)
    RetornoNeg <- sum(sinal == -1)
    ProbPos <- RetornoPos/(RetornoPos + RetornoNeg)
    ProbPos

    #ProbabilidadeTmedia =pt(mean(prev),
    #                      df=length(prev)-1,lower.tail=FALSE)
    # Calculo do erro-padrao
    std.error= function(x) {
      sd(x)/sqrt(length(x))
    }
    se = std.error(prev)

    # Calculo da probabilidade excesso de retorno >0 c/ deslocamento da curva T
    if (mean(prev)>0) {
      ProbabilidadeTmedia =pt(0.0,
                              df=length(prev)-1,ncp = se, lower.tail=FALSE)
      cat("Densidade Assimetrica ? Direita (Negativa)")
    } else {
      ProbabilidadeTmedia =pt(0.0,
                              df=length(prev)-1,ncp = -se, lower.tail=FALSE)
      cat("Densidade Assimetrica ? Esquerda (Positiva)")
    }
    #ProbabilidadeTmedia =pt(mean(prev),
    #                    df=length(prev)-1,lower.tail=TRUE)

    # Resultados das Probabilidades

    ResProbPosNNet[1,k]= ProbPos
    ResProbTPosNNet[1,k]= ProbabilidadeTmedia
    #############################################################################
    ## REDE NEURAL PARTICULAR

    #############################################################################

    # Acrescentando o Bias ao Banco de dados

    #BIAS da camada de entrada
    #for (i in 1:  nlinhas){
    # treinamento["BIAS"]<-1
    #}


    #View(saidas)
    ncolunas <- ncol(entradas)

    # Gerando pesos iniciais aleat?rios
    pesos0 = matrix(runif(ncolunas*nlinhas, min = 0, max = 1), nrow = ncolunas,
                    ncol = nlinhas, byrow = T)
    pesos1 = matrix(runif(ncolunas*(nlinhas), min = 0, max = 1), nrow = nlinhas,
                    ncol = 1, byrow = T)

    ### pesos1 com Bias na camada oculta
    #pesos1 = matrix(runif(ncolunas*(nlinhas+1), min = 0, max = 1),
    #nrow = nlinhas+1, ncol = 1, byrow = T)
    #View(pesos0)

    # Gerando pesos iniciais aleat?rios PARAMETRIZADOS (Revisar)
    #repeat {
    #pesos0 = matrix(runif(ncolunas, min = 0, max = 1, sum(ncolunas)=1),
    #nrow = ncolunas+1, ncol = 1, byrow = T)
    #until
    #sum(pesos0)=1
    #}

    # Gerando pesos iniciais carteira ing?nua (pesos iguais)
    #pesos0 = matrix(1/ncol(entradas), nrow = ncolunas, ncol = nlinhas, byrow = T)
    #pesos1 = matrix(runif(ncolunas*nlinhas, min = -1, max = 1), nrow = nlinhas+1,
    #ncol = 1, byrow = T)
    #View(pesos0)


    # Fun??o Sigmoide
    sigmoide = function(soma) {
      #return (1/ (1+exp(-soma)))
      #Fun??o Tangente Hiperb?lica
      #return (1-tanh(soma))
      return (tanh(soma))
    }

    # Derivada da fun??o Sigmoide
    sigmoideDerivada = function(sig) {
      return (sig * (1-sig))
    }

    # Estimando o n?mero de ?pocas e a taxa de aprendizagem
    epocas = 2500
    momento = 1
    taxaAprendizagem = 0.3
    #########################################
    for(j in 1:epocas) {
      # fed forward
      camadaEntrada = as.matrix(entradas)
      somaSinapse0 = camadaEntrada %*% pesos0
      camadaOculta = sigmoide(somaSinapse0)

      ### Introduzindo Bias na segunda camada
      #BIAS2 = NULL
      # for (i in 1:  nlinhas){
      #   BIAS2[i] <- 1
      #}
      #BIAS2 <- data.frame(BIAS2)
      #camadaOculta <- cbind(camadaOculta,BIAS2)
      #camadaOculta = as.matrix(camadaOculta)


      somaSinapse1 = camadaOculta %*% pesos1
      camadaSaida = sigmoide(somaSinapse1)

      # back forward
      #erroCamadaSaida = 1 - saidas - camadaSaida # M?xima diferen?a
      erroCamadaSaida = saidas - camadaSaida # M?nima diferen?a
      mediaAbsoluta = mean(abs(erroCamadaSaida))
      print(paste('Erro:', mediaAbsoluta))

      derivadaSaida = sigmoideDerivada(camadaSaida)
      deltaSaida = erroCamadaSaida * derivadaSaida

      #deltaSaidaXPeso = deltaSaida %*% pesos1 # Matrizes com dimens?es diferentes,
      #por isso da erro, ? preciso transpor a matriz pesos1
      pesos1Transposta = t(pesos1)
      deltaSaidaXPeso = deltaSaida %*% pesos1Transposta
      deltaCamadaOculta = deltaSaidaXPeso * sigmoideDerivada(camadaOculta)

      # (backpropagation)
      # Atualiza??o dos pesos da camada de sa?da at? a oculta
      camadaOcultaTransposta = t(camadaOculta)
      pesosNovo1 = camadaOcultaTransposta %*% deltaSaida
      pesos1 = (pesos1 * momento) + (pesosNovo1 * taxaAprendizagem)

      # Atualiza??o dos pesos da camada oculta at? a de entrada
      camadaEntradaTransposta = t(camadaEntrada)
      pesosNovo0 = camadaEntradaTransposta %*% deltaCamadaOculta
      pesos0 = (pesos0 * momento) + (pesosNovo0[,-(nlinhas+1)] * taxaAprendizagem)

    }

    hist(camadaSaida,
         main = paste("Histograma Previs?es RNA Fase de Treinamento - Ativo",
                      xnames= nome),
         xlab = paste("Retorno Excedente sobre", xnames = "RM"))
    mean(camadaSaida)
    median(camadaSaida)
    sd(camadaSaida)
    std.error= function(x) {
      sd(x)/sqrt(length(x))
    }
    se = std.error(camadaSaida)
    # Probabildiade Normal
    pnorm(q = 0.01, # probabilidade do retorno ser superior a 0.1%
          mean = mean(camadaSaida), # media
          sd = sd(camadaSaida), # desvio-pedrao
          lower.tail = FALSE) # Calcula P[X >x]
    ## Probabilidade com curtorese (Lambda > 3) = Probabilidade t Student
    library(moments)
    #png(file = "leptokurtic.png")

    if (kurtosis(camadaSaida)>3) {
      print("Curtose da curva Leptoc?rtica:")
      print(kurtosis(camadaSaida))
    }
    t = 3.373 # p =0.1
    q = -0.4
    ProbtStudent = (q-mean(camadaSaida))/(std.error(camadaSaida))
    #ProbtStudent
    ProbStudent = t*(std.error(camadaSaida)) + mean(camadaSaida)
    #ProbStudent

    # Calculo da probabilidade excesso de retorno >0 c/ deslocamento da curva T
    #ProbabilidadeTmedia =pt(mean(camadaSaida),
    #                       df=length(camadaSaida)-1, lower.tail=FALSE)

    if (mean(camadaSaida)>0) {
      ProbabilidadeTmedia =pt(0.0,
                              df=length(camadaSaida)-1,ncp = se, lower.tail=FALSE)
      cat("Densidade Assimetrica ? Direita (Negativa)")
    } else {
      ProbabilidadeTmedia =pt(0.0,
                              df=length(camadaSaida)-1,ncp = -se, lower.tail=FALSE)
      cat("Densidade Assim?trica ? Esquerda (Positiva)")
    }
    #ProbabilidadeTmedia =pt(mean(camadaSaida),
    #                 df=length(camadaSaida)-1, lower.tail = TRUE)

    Probabilidadest = NULL
    for (l in 1:nlinhas){
      Probabilidadest[l] = pt(camadaSaida[l],
                              df=length(camadaSaida)-1, lower.tail=FALSE)
    }

    # Plot RM e a rede neural artificial no teste
    library(data.table)
    Data <- as.data.table(camadaSaida, keep.rownames = TRUE)
    Data <- as.data.frame(Data)

    nome = colnames(entradas)[1]
    matplot(cbind(camadaSaida, saidas), type = "l", xaxt = "n",
            xlab = "Data",
            ylab = "Retorno",
            main = paste("Otimiza??o RNA Fase de Treinamento - Ativo",
                         xname = nome))
    legend("topright", legend = c("RNA", nome), pch = 19, col = c("black", "red"))
    axis(1, 1:nlinhas, Data$rn)

    ## Sinal Maior que a media
    #sinal = NULL
    #for (i in 1:nlinhas) {
    #  if(camadaSaida[i] > mean(camadaSaida)){
    #    sinal[i] = 1
    #  }
    #  else {
    #    sinal[i] = -1
    #  }
    #}
    #sinal

    ## Sinal POSITIVO
    sinal = NULL
    for (i in 1:nlinhas) {
      if(camadaSaida[i] > 0){
        sinal[i] = 1
      }
      else {
        sinal[i] = -1
      }
    }
    sinal

    ### Probabilidades
    RetornoPos <- sum(sinal ==1)
    RetornoNeg <- sum(sinal == -1)
    ProbPos <- RetornoPos/(RetornoPos + RetornoNeg)
    #ProbPos

    # Resultados das Probabilidades

    ResProbPos[1,k]= ProbPos
    ResProbTPos[1,k]= ProbabilidadeTmedia

    #dev.off() ### Salvando gr?ficos anteriores

    ## Exibindo histograma
    #hist(camadaSaida,
    #    main = paste("Histograma Previs?es RNA Fase de Treinamento - Ativo",
    #                xnames= nome),
    #  xlab = paste("Retorno Excedente sobre", xnames = RM))

    ############################################################################
    ############################################################################
    ############################################################################
    ################################## Fase de Teste ###########################
    ############################################################################
    ############################################################################
    ############################################################################



    ####################### Amostra de Teste ###################################
    # Criando as vari?veis como vetor para teste- com o restantes da amostra que
    # excede o Tratamento at? o ?ltimo preg?o
    I_dataPredict = F_data+1
    F_dataPredict = nrow(dat_r)-1
    entradasPredict = as.matrix(dat_r[I_dataPredict:F_dataPredict,])
    saidasPredict = as.matrix(dat_r[(I_dataPredict+1):(F_dataPredict+1),1])


    # Criando as vari?veis como vetor para teste - com datas espec?ficas
    #Inicio_dataPredict = "2022-01-04"
    #Fim_dataPredict = "2022-06-23"
    #I_dataPredict = which(rownames(dat_r)==Inicio_dataPredict)
    #F_dataPredict = which(rownames(dat_r)==Fim_dataPredict)
    #entradasPredict = as.matrix(dat_r[I_dataPredict:F_dataPredict,])
    #saidasPredict = as.matrix(dat_r[(I_dataPredict+1):(F_dataPredict+1),1])


    #####
    # Criando as vari?veis como vetor para teste -50% dos dados
    #comprimentoPredict = ceiling(0.5*nrow(dat_r)) # ceiling arredonda para mais
    #testePredict=as.data.frame(dat_r)
    #nAtivosPredict = ncol(dados)
    #entradasPredict = as.matrix(testePredict[1:comprimentoPredict,])
    #saidas = as.matrix(testePredict[2:(comprimentoPredict+1),1])
    #I_data = entradasPredict[1]
    #F_data = entradasPredict[length(entradasPredict)]

    #####
    nlinhasPredict <- nrow(entradasPredict)
    ncolunasPredict <- ncol(entradasPredict)


    ## Previs?o
    prevPredict = predict(nn, entradasPredict)
    nome = colnames(entradasPredict)[1]

    plot(as.vector(entradasPredict[,1]), type="l", col = "red",
         main = paste("Retornos Amostra de Teste", xnames = nome))
    legend("topright", legend = c("RNA", nome), pch = 19,
           col = c("black", "red"))
    lines(prevPredict)


    hist(prevPredict,
         main = paste("Histograma Previs?es RNA Fase de Teste - Ativo",
                      xnames= nome),
         xlab = paste("Retorno Excedente sobre", xnames = "RM"))
    mean(prevPredict)

    TestesPredict = compute(nn, entradasPredict)
    TestesPredict$net.result
    hist(TestesPredict$net.result,
         main = paste("Histograma Previs?es RNA Fase de Teste - Ativo",
                      xnames= nome), xlab = paste("Retorno Excedente sobre",
                                                  xnames = "RM"))
    ## Avaliando a acur?cia (ME, RMSE, MAE, MPE e MAPE)
    dat_r2Predict = na.fill(entradasPredict, "extend")
    prev2Predict = na.fill(entradasPredict, "extend")
    accuracy(as.vector(prev2Predict),entradas[1])

    sinal = NULL
    nlinhasPredict = nrow(entradasPredict)
    for (i in 1:nlinhasPredict) {
      if(prevPredict[i] > 0){
        sinal[i] = 1
      }
      else {
        sinal[i] = -1
      }
    }
    sinal
    ### Probabilidades
    RetornoPos <- sum(sinal ==1)
    RetornoNeg <- sum(sinal == -1)
    ProbPos <- RetornoPos/(RetornoPos + RetornoNeg)
    ProbPos

    #ProbabilidadeTmedia =pt(mean(prev),
    #                      df=length(prev)-1,lower.tail=FALSE)
    # Calculo do erro-padrao
    std.error= function(x) {
      sd(x)/sqrt(length(x))
    }
    se = std.error(prevPredict)

    # Calculo da probabilidade excesso de retorno >0 c/ deslocamento da curva T
    if (mean(prevPredict)>0) {
      ProbabilidadeTmedia =pt(0.0,
                              df=length(prevPredict)-1,ncp = se,
                              lower.tail=FALSE)
      cat("Densidade Assimetrica ? Direita (Negativa)")
    } else {
      ProbabilidadeTmedia =pt(0.0,
                              df=length(prevPredict)-1,ncp = -se,
                              lower.tail=FALSE)
      cat("Densidade Assimetrica ? Esquerda (Positiva)")
    }
    #ProbabilidadeTmedia =pt(mean(prev),
    #                    df=length(prev)-1,lower.tail=TRUE)

    # Resultados das Probabilidades

    ResProbPosNNetPredict[1,k]= ProbPos
    ResProbTPosNNetPredict[1,k]= ProbabilidadeTmedia

    ####### Carteira Particular#################
    # fed forward
    camadaEntradaPredict = as.matrix(entradasPredict)
    somaSinapse0Predict = camadaEntradaPredict %*% pesos0
    camadaOcultaPredict = sigmoide(somaSinapse0Predict)


    somaSinapse1Predict = camadaOcultaPredict %*% pesos1
    camadaSaidaPredict = sigmoide(somaSinapse1Predict)


    hist(camadaSaidaPredict,
         main = paste("Histograma Previs?es RNA Fase de Teste - Ativo",
                      xnames= nome),
         xlab = paste("Retorno Excedente sobre", xnames = "RM"))
    mean(camadaSaidaPredict)
    median(camadaSaidaPredict)
    sd(camadaSaidaPredict)
    std.error= function(x) {
      sd(x)/sqrt(length(x))
    }
    se = std.error(camadaSaidaPredict)
    # Probabildiade Normal
    pnorm(q = 0.01, # probabilidade do retorno ser superior a 0.1%
          mean = mean(camadaSaidaPredict), # media
          sd = sd(camadaSaidaPredict), # desvio-pedrao
          lower.tail = FALSE) # Calcula P[X >x]
    ## Probabilidade com curtorese (Lambda > 3) = Probabilidade t Student
    library(moments)
    #png(file = "leptokurtic.png")

    if (kurtosis(camadaSaidaPredict)>3) {
      print("Curtose da curva Leptoc?rtica:")
      print(kurtosis(camadaSaidaPredict))
    }
    t = 3.373 # p =0.1
    q = -0.4
    ProbtStudentPredict=(q-mean(camadaSaidaPredict))/(std.error(camadaSaidaPredict))
    #ProbtStudent
    ProbStudentPredict= t*(std.error(camadaSaidaPredict)) + mean(camadaSaidaPredict)
    #ProbStudent

    # Calculo da probabilidade excesso de retorno >0 c/ deslocamento da curva T
    #ProbabilidadeTmedia =pt(mean(camadaSaida),
    #                       df=length(camadaSaida)-1, lower.tail=FALSE)

    if (mean(camadaSaidaPredict)>0) {
      ProbabilidadeTmedia =pt(0.0,
                              df=length(camadaSaidaPredict)-1,ncp = se,
                              lower.tail=FALSE)
      cat("Densidade Assimetrica ? Direita (Negativa)")
    } else {
      ProbabilidadeTmedia =pt(0.0,
                              df=length(camadaSaidaPredict)-1,ncp = -se,
                              lower.tail=FALSE)
      cat("Densidade Assim?trica ? Esquerda (Positiva)")
    }
    #ProbabilidadeTmedia =pt(mean(camadaSaida),
    #                 df=length(camadaSaida)-1, lower.tail = TRUE)

    Probabilidadest = NULL
    for (l in 1:nlinhasPredict){
      Probabilidadest[l] = pt(camadaSaidaPredict[l],
                              df=length(camadaSaidaPredict)-1, lower.tail=FALSE)
    }

    # Plot benchmark e a rede neural artificial no teste
    library(data.table)
    DataPredict <- as.data.table(camadaSaidaPredict, keep.rownames = TRUE)
    DataPredict <- as.data.frame(DataPredict)

    matplot(cbind(camadaSaidaPredict, saidasPredict), type = "l", xaxt = "n",
            xlab = "Data",
            ylab = "Retorno",
            main = paste("Otimiza??o RNA Fase de Teste - Ativo", xname = nome))
    legend("topright", legend = c("RNA", nome), pch = 19,
           col = c("black", "red"))
    axis(1, 1:nlinhasPredict, DataPredict$rn)

    ## Sinal Maior que a media
    #sinal = NULLnlinhasPredict
    #for (i in 1:nlinhasPredict) {
    #  if(camadaSaidaPredict[i] > mean(camadaSaidaPredict)){
    #    sinal[i] = 1
    #  }
    #  else {
    #    sinal[i] = -1
    #  }
    #}
    #sinal

    ## Sinal POSITIVO
    sinal = NULL
    for (i in 1:nlinhasPredict) {
      if(camadaSaidaPredict[i] > 0){
        sinal[i] = 1
      }
      else {
        sinal[i] = -1
      }
    }
    sinal

    ### Probabilidades
    RetornoPos <- sum(sinal ==1)
    RetornoNeg <- sum(sinal == -1)
    ProbPos <- RetornoPos/(RetornoPos + RetornoNeg)
    #ProbPos

    # Resultados das Probabilidades

    ResProbPosPredict[1,k]= ProbPos
    ResProbTPosPredict[1,k]= ProbabilidadeTmedia



    dev.off() ### Salvando gr?ficos do Ativo dentro Loop

    #############################Fim Amostra de Teste###########################


  }




  ################################################################################
  ############################Fim do envelope#####################################
  ################################################################################







  ResProbPosNNet   #Resultado Probabilidade Sinal Positivo - NeuralNet Train
  ResProbPos       #Resultado Probabilidade Sinal Positivo - RNA Particular Train
  ResProbTPosNNet  #Resultado Probabilidade Distribui??o t - NeuralNet Train
  ResProbTPos      #Resultado Probabilidade Distribui??o t - RNA Particular Train

  ResProbPosNNetPredict #Resultado Probabilidade Sinal Positivo - NeuralNet Test
  ResProbPosPredict #Resultado Probabilidade Sinal Positivo - RNA Particular Test
  ResProbTPosNNetPredict #Resultado Probabilidade Distribui??o t - NeuralNet Test
  ResProbTPosPredict #Resultado Probabilidade Distribui??o t - RNA Particular Test
  ###
  write_xlsx (ResProbPosNNet,"ResProbPosNNet.xlsx")
  write_xlsx (ResProbPos,"ResProbPos.xlsx")
  write_xlsx (ResProbTPosNNet, "ResProbTPosNNet.xlsx")
  write_xlsx (ResProbTPos, "ResProbTPos.xlsx")

  write_xlsx (ResProbPosNNetPredict, "ResProbPosNNetPredict.xlsx")
  write_xlsx (ResProbPosPredict, "ResProbPosPredict.xlsx")
  write_xlsx (ResProbTPosNNetPredict, "ResProbTPosNNetPredict.xlsx")
  write_xlsx (ResProbTPosPredict, "ResProbTPosPredict.xlsx")


  ################################################################################
  ################################################################################
  ########################          AMOSTRAS          ############################
  ################################################################################
  ################################################################################

  ## Train NNet
  order(as.matrix(ResProbPosNNet))
  nomes = colnames(ResProbPosNNet)
  prob = t(ResProbPosNNet)
  Res=as.matrix(ResProbPosNNet)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TesteNNet= matrix(data=ncol(ResProbPosNNet):1, nrow=1, ncol=ncol(ResProbPosNNet))
  colnames(TesteNNet)= Test$Nomes
  TesteNNet[1,]=Test$Prob

  ###

  order(as.matrix(ResProbPos))
  nomes = colnames(ResProbPos)
  prob = t(ResProbPos)
  Res=as.matrix(ResProbPos)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TestePos= matrix(data=ncol(ResProbPos):1, nrow=1, ncol=ncol(ResProbPos))
  colnames(TestePos)= Test$Nomes
  TestePos[1,]=Test$Prob

  ###

  order(as.matrix(ResProbTPosNNet))
  nomes = colnames(ResProbTPosNNet)
  prob = t(ResProbTPosNNet)
  Res=as.matrix(ResProbTPosNNet)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TesteTNNet= matrix(data=ncol(ResProbTPosNNet):1, nrow=1, ncol=ncol(ResProbTPosNNet))
  colnames(TesteTNNet)= Test$Nomes
  TesteTNNet[1,]=Test$Prob

  ###

  order(as.matrix(ResProbTPos))
  nomes = colnames(ResProbTPos)
  prob = t(ResProbTPos)
  Res=as.matrix(ResProbTPos)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TesteTPos= matrix(data=ncol(ResProbTPos):1, nrow=1, ncol=ncol(ResProbTPos))
  colnames(TesteTPos)= Test$Nomes
  TesteTPos[1,]=Test$Prob


  ###
  ### Test

  order(as.matrix(ResProbPosNNetPredict))
  nomes = colnames(ResProbPosNNetPredict)
  prob = t(ResProbPosNNetPredict)
  Res=as.matrix(ResProbPosNNetPredict)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TesteNNetPredict= matrix(data=ncol(ResProbPosNNetPredict):1, nrow=1, ncol=ncol(ResProbPosNNetPredict))
  colnames(TesteNNetPredict)= Test$Nomes
  TesteNNetPredict[1,]=Test$Prob

  ###


  order(as.matrix(ResProbPosPredict))
  nomes = colnames(ResProbPosPredict)
  prob = t(ResProbPosPredict)
  Res=as.matrix(ResProbPosPredict)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TestePosPredict= matrix(data=ncol(ResProbPosPredict):1, nrow=1, ncol=ncol(ResProbPosPredict))
  colnames(TestePosPredict)= Test$Nomes
  TestePosPredict[1,]=Test$Prob

  ###


  order(as.matrix(ResProbTPosNNetPredict))
  nomes = colnames(ResProbTPosNNetPredict)
  prob = t(ResProbTPosNNetPredict)
  Res=as.matrix(ResProbTPosNNetPredict)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TesteTNNetPredict= matrix(data=ncol(ResProbTPosNNetPredict):1, nrow=1, ncol=ncol(ResProbTPosNNetPredict))
  colnames(TesteTNNetPredict)= Test$Nomes
  TesteTNNetPredict[1,]=Test$Prob

  ###
  #### RNA-t Particular para armazenar em T8

  order(as.matrix(ResProbTPosPredict))
  nomes = colnames(ResProbTPosPredict)
  prob = t(ResProbTPosPredict)
  Res=as.matrix(ResProbTPosPredict)
  Res2 = data.frame(Nomes=nomes,Prob=Res[1,])
  Test = arrange(Res2, desc(Prob))
  TesteTPosPredict= matrix(data=ncol(ResProbTPosPredict):1, nrow=1, ncol=ncol(ResProbTPosNNetPredict))
  colnames(TesteTPosPredict)= Test$Nomes
  TesteTPosPredict[1,]=Test$Prob



  ###
  #T1=data.frame(TesteNNet)   #Resultado Sinal Positivo - NeuralNet Ordenada Train
  #T2=data.frame(TesteTNNet)  #Resultado Sinal Positivo - RNA Particular Ord Train
  #T3=data.frame(TestePos)    #Resultado Prob. Dist t - NeuralNet Ordenada Train
  #T4=data.frame(TesteTPos)   #Resultado Proba. Dist t - RNA Particular Ord Train
  #T5=data.frame(TesteNNetPredict) #Res Prob. Sinal Positivo - NeuralNet Ord Test
  #T6=data.frame(TestePosPredict) #Resultado Prob. Dist t - NeuralNet Ordenada Test
  #T7=data.frame(TesteTNNetPredict) #Res Prob. Sinal Positivo - RNA Part Ord Test
  #T8=data.frame(TesteTPosPredict) #Res Prob. Dist t - RNA Particular Ordenada Test

  ###
  T1=data.frame(TesteNNet)   #Resultado Sinal Positivo - NeuralNet Ordenada Train
  T2=data.frame(TesteTNNet)  #Resultado Prob. Dist t - NeuralNet Ordenada Train
  T3=data.frame(TestePos)    #Resultado Sinal Positivo - RNA Particular Ord Train
  T4=data.frame(TesteTPos)   #Resultado Proba. Dist t - RNA Particular Ord Train
  T5=data.frame(TesteNNetPredict) #Res Prob. Sinal Positivo - NeuralNet Ord Test
  T6=data.frame(TestePosPredict) #Res Prob. Sinal Positivo - RNA Part Ord Test
  T7=data.frame(TesteTNNetPredict) #Resultado Prob. Dist t - NeuralNet Ordenada Test
  T8=data.frame(TesteTPosPredict) #Res Prob. Dist t - RNA Particular Ordenada Test

  save(T8,file='~/Assets_ANNt_Order.rda')
View(T8)
write_xlsx(T8, "~/Assets_ANNt_Order.xlsx")
  ###############################
}
