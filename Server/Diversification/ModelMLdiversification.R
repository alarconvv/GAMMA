##############################################################################
# Diversity: Models ML
##############################################################################

#Temporal objects
#
DiverModML <- reactiveValues()
DiverModML$iterObjectDiver <- NULL
DiverModML$runObjModels <- NULL
DiverModML$YuleList <-list()
DiverModML$iteryule<-list()
DiverModML$BDList <-list()
DiverModML$iterBD<-list()
DiverModML$BDvarSpeList <-list()
DiverModML$iterBDvarSpe<-list()
DiverModML$BDvarExtList <-list()
DiverModML$iterBDvarExt<-list()
DiverModML$BDvarSpeExtList <-list()
DiverModML$iterBDvarSpeExt<-list()
DiverModML$DiverDepentModMLList <-list()
DiverModML$iterDiverDepentModML<-list()
DiverModML$modelList <- NULL

DiverModML$countyule <- 0
DiverModML$countBD <- 0
DiverModML$countBDvarSpe <- 0
DiverModML$countBDvarExt <- 0
DiverModML$countBDvarSpeExt <- 0
DiverModML$countDiverDepentModML <- 0


DiverModML$Result <- NULL
DiverModML$iterYuleResult <- NULL
DiverModML$ResultYuleList  <- NULL
DiverModML$countYuleResult <- 0


DiverModML$iterBDResult <- NULL
DiverModML$ResultBDList  <- NULL
DiverModML$countBDResult <- 0

DiverModML$iterBDvarSpeResult <- NULL
DiverModML$ResultBDvarSpeList  <- NULL
DiverModML$countBDvarSpeResult <- 0


DiverModML$iterBDvarExtResult <- NULL
DiverModML$ResultBDvarExtList  <- NULL
DiverModML$countBDvarExtResult <- 0

DiverModML$iterBDvarSpeExtResult <- NULL
DiverModML$ResultBDvarSpeExtList  <- NULL
DiverModML$countBDvarSpeExtResult <- 0

DiverModML$iterDiverDepentModelResult <- NULL
DiverModML$ResultDiverDepentModelList  <- NULL
DiverModML$countDiverDepentModelResult <- 0


#Render print in Info panel: Models: ML
#
output$infoPanelDiverModML <- renderPrint( {
  print(DiverModML$iterObjectDiver)
})


## Tree 


  #Get tree from DT
  
treeModML <- reactive({
  
  validate(need(try(treeInputDiver5()),"Please, your tree must be ultrametric and binary ")
  )
  treeInputDiver5()
}
)



#plot


#Plot tree
#
heightDiverModML <- reactive(input$PlotHeightDiverModML[1])
widthDiverModML <- reactive(input$PlotWidthDiverModML[1])

output$PhyloPlotDiver5 <- renderPlot( height = heightDiverModML  , width = widthDiverModML,{
  req(treeModML())
  
  
  plot.phylo(treeModML(), show.tip.label = T,
             cex = input$tipSizeDiverModML[1], use.edge.length = T,
             edge.width = 0.8,edge.color = 'grey40')
})






# Yule Model

observeEvent(input$yuleModML,{
  DiverModML$runObjModels$Yule <- NULL
})

observeEvent(input$addYuleModML,{
 
  DiverModML$iteryule$rho <- as.numeric(input$fractYuleModML)
  DiverModML$iteryule$rate <- as.numeric(input$BrateYuleModML)
  DiverModML$YuleList$model <- DiverModML$iteryule
    
    DiverModML$countyule <- DiverModML$countyule + 1
    
    names(DiverModML$YuleList)[DiverModML$countyule] <- paste('Yule',DiverModML$countyule, sep = '')
    
    DiverModML$modelList <- c(DiverModML$modelList,paste('Yule',DiverModML$countyule, sep = ''))
    
    DiverModML$runObjModels$Yule <- DiverModML$YuleList
    
})

#Temporal object to print in info panel
# info:  models
observeEvent(input$addYuleModML, {
  
  DiverModML$iterObjectDiver <- DiverModML$runObjModels
  
  updateSelectInput(session = session,inputId = 'modelsFitModML',label = 'Models', choices = as.character( DiverModML$modelList),selected = as.character( DiverModML$modelList))
})



# BD Model

observeEvent(input$BDContModML,{
  DiverModML$runObjModels$BD <- NULL
  
  ratesBDTable <- matrix(data = c(0.1,0.1),nrow = 1,ncol = 2)
  colnames(ratesBDTable) <- c('Sp', 'Ext')
  row.names(ratesBDTable) <- 'Rates'
  
  output$BDrateModML <- renderRHandsontable({rhandsontable(ratesBDTable, readOnly = F)})
})

observeEvent(input$addBDContModML,{
  DiverModML$iterBD$rho <- as.numeric(input$fractBDContModML)
  DiverModML$iterBD$Rates <- as.numeric(hot_to_r(input$BDrateModML))
  DiverModML$BDList$model <- DiverModML$iterBD
  
  DiverModML$countBD <- DiverModML$countBD + 1
  
  names(DiverModML$BDList)[DiverModML$countBD] <- paste('BD',DiverModML$countBD, sep = '')
  
  DiverModML$modelList <- c(DiverModML$modelList,paste('BD',DiverModML$countBD, sep = ''))
  
  DiverModML$runObjModels$BD <- DiverModML$BDList
  
  
})


#Temporal object to print in info panel
# info:  models
observeEvent(input$addBDContModML, {
  DiverModML$iterObjectDiver <- DiverModML$runObjModels
  
  updateSelectInput(session = session,inputId = 'modelsFitModML',label = 'Models', choices = as.character(DiverModML$modelList),selected = as.character( DiverModML$modelList))
})



# BDvarSp Model

observeEvent(input$distBDvarSpeModML,{
  DiverModML$runObjModels$BDvarSpe <- NULL
  
  if (input$distBDvarSpeModML == 'linear.t'){
    ratesBDvarSpeTable <- matrix(data = c(0.01,0.01,0.1),nrow = 1,ncol = 3)
    colnames(ratesBDvarSpeTable) <- c('Sp.c', 'Sp.m','Ext')
    row.names(ratesBDvarSpeTable) <- 'Rates'
    output$RateBDvarSpeModML <- renderRHandsontable({rhandsontable(ratesBDvarSpeTable, readOnly = F)})
  }

  if (input$distBDvarSpeModML == 'stepf.t'){
    ratesBDvarSpeTable <- matrix(data = c(0.01,0.01,0.01,0.1),nrow = 1,ncol = 4)
    colnames(ratesBDvarSpeTable) <- c('Sp.Y0', 'Sp.Y1', 'Sp.tc','Ext')
    row.names(ratesBDvarSpeTable) <- 'Rates'
    output$RateBDvarSpeModML <- renderRHandsontable({rhandsontable(ratesBDvarSpeTable, readOnly = F)})
  }
  
  if (input$distBDvarSpeModML == 'exp.t'){
    ratesBDvarSpeTable <- matrix(data = c(0.01,0.01,0.1),nrow = 1,ncol = 3)
    colnames(ratesBDvarSpeTable) <- c('Sp.l', 'Sp.a','Ext')
    row.names(ratesBDvarSpeTable) <- 'Rates'
    output$RateBDvarSpeModML <- renderRHandsontable({rhandsontable(ratesBDvarSpeTable, readOnly = F)})
  }
  
  if (input$distBDvarSpeModML == 'sigmoid.t'){
    ratesBDvarSpeTable <- matrix(data = c(0.01,0.01,0.01,0.01,0.1),nrow = 1,ncol = 5)
    colnames(ratesBDvarSpeTable) <- c('Sp.Y0', 'Sp.Y1','Sp.tmid', 'Sp.r','Ext')
    row.names(ratesBDvarSpeTable) <- 'Rates'
    output$RateBDvarSpeModML <- renderRHandsontable({rhandsontable(ratesBDvarSpeTable, readOnly = F)})
  }
  
  if (input$distBDvarSpeModML == 'spline.t'){
    
  }
})


observeEvent(input$addBDvarSpeModML,{
  DiverModML$iterBDvarSpe$rho <- as.numeric(input$fractBDvarSpeModML)
  DiverModML$iterBDvarSpe$Distribution <- input$distBDvarSpeModML
  DiverModML$iterBDvarSpe$Rates <- as.numeric(hot_to_r(input$RateBDvarSpeModML))

  DiverModML$BDvarSpeList$model <- DiverModML$iterBDvarSpe
  
  DiverModML$countBDvarSpe <- DiverModML$countBDvarSpe + 1
  
  names(DiverModML$BDvarSpeList)[DiverModML$countBDvarSpe] <- paste('BDvarSpe',DiverModML$countBDvarSpe, sep = '')
  
  DiverModML$modelList <- c(DiverModML$modelList, paste('BDvarSpe',DiverModML$countBDvarSpe, sep = ''))
  
  DiverModML$runObjModels$BDvarSpe <- DiverModML$BDvarSpeList
  
  
  
})


#Temporal object to print in info panel
# info:  models
observeEvent(input$addBDvarSpeModML, {
  
  DiverModML$iterObjectDiver <- DiverModML$runObjModels
  
  updateSelectInput(session = session,inputId = 'modelsFitModML',label = 'Models', choices = as.character(DiverModML$modelList),selected = as.character( DiverModML$modelList) )
})


# BDvarExt Model

observeEvent(input$distBDvarExtModML,{
  DiverModML$runObjModels$BDvarExt <- NULL
  
  if (input$distBDvarExtModML == 'linear.t'){
    ratesBDvarExtTable <- matrix(data = c(0.1,0.01,0.01),nrow = 1,ncol = 3)
    colnames(ratesBDvarExtTable) <- c('Sp','Ext.c', 'Ext.m')
    row.names(ratesBDvarExtTable) <- 'Rates'
    output$RateBDvarExtModML <- renderRHandsontable({rhandsontable(ratesBDvarExtTable, readOnly = F)})
  }
  
  if (input$distBDvarExtModML == 'stepf.t'){
    ratesBDvarExtTable <- matrix(data = c(0.1,0.01,0.01,0.01),nrow = 1,ncol = 4)
    colnames(ratesBDvarExtTable) <- c('Sp','Ext.Y0','Ext.Y1','Ext.tc')
    row.names(ratesBDvarExtTable) <- 'Rates'
    output$RateBDvarExtModML <- renderRHandsontable({rhandsontable(ratesBDvarExtTable, readOnly = F)})
  }
  
  if (input$distBDvarExtModML == 'exp.t'){
    ratesBDvarExtTable <- matrix(data = c(0.1,0.01,0.01),nrow = 1,ncol = 3)
    colnames(ratesBDvarExtTable) <- c('Sp','Ext.l', 'Ext.a')
    row.names(ratesBDvarExtTable) <- 'Rates'
    output$RateBDvarExtModML <- renderRHandsontable({rhandsontable(ratesBDvarExtTable, readOnly = F)})
  }
  
  if (input$distBDvarExtModML == 'sigmoid.t'){
    ratesBDvarExtTable <- matrix(data = c(0.1,0.01,0.01,0.01,0.01),nrow = 1,ncol = 5)
    colnames(ratesBDvarExtTable) <- c('Sp','Ext.Y0', 'Ext.Y1','Ext.tmid', 'Ext.r')
    row.names(ratesBDvarExtTable) <- 'Rates'
    output$RateBDvarExtModML <- renderRHandsontable({rhandsontable(ratesBDvarExtTable, readOnly = F)})
  }
  
  if (input$distBDvarExtModML == 'spline.t'){
    
  }
})


observeEvent(input$addBDvarExtModML,{
  DiverModML$iterBDvarExt$rho <- as.numeric(input$fractBDvarExtModML)
  DiverModML$iterBDvarExt$Distribution <- input$distBDvarExtModML
  DiverModML$iterBDvarExt$Rates <- as.numeric(hot_to_r(input$RateBDvarExtModML))
  
  DiverModML$BDvarExtList$model <- DiverModML$iterBDvarExt
  
  DiverModML$countBDvarExt <- DiverModML$countBDvarExt + 1
  
  names(DiverModML$BDvarExtList)[DiverModML$countBDvarExt] <- paste('BDvarExt',DiverModML$countBDvarExt, sep = '')
  
  DiverModML$modelList <- c(DiverModML$modelList , paste('BDvarExt',DiverModML$countBDvarExt, sep = ''))
  
  DiverModML$runObjModels$BDvarExt <- DiverModML$BDvarExtList
  
  
})


#Temporal object to print in info panel
# info:  models
observeEvent(input$addBDvarExtModML, {
  DiverModML$iterObjectDiver <- DiverModML$runObjModels
  
  updateSelectInput(session = session,inputId = 'modelsFitModML',label = 'Models', choices = as.character(DiverModML$modelList),selected = as.character( DiverModML$modelList) )
})


# BDvarSpeExt Model

observeEvent(input$distBDvarSpeExtModMLsp,{
  DiverModML$runObjModels$BDvarSpeExt <- NULL
  
  #Speciation rate
  if (input$distBDvarSpeExtModMLsp == 'linear.t'){
    ratesBDvarSpeExtTablesp <- matrix(data = c(0.01,0.01),nrow = 1,ncol = 2)
    colnames(ratesBDvarSpeExtTablesp) <- c('Sp.c', 'Sp.m')
    row.names(ratesBDvarSpeExtTablesp) <- 'Rates'
    output$RateBDvarSpeExtModMLsp <- renderRHandsontable({rhandsontable(ratesBDvarSpeExtTablesp, readOnly = F)})
  }
  
  if (input$distBDvarSpeExtModMLsp == 'stepf.t'){
    ratesBDvarSpeExtTablesp <- matrix(data = c(0.01,0.01,0.01),nrow = 1,ncol = 3)
    colnames(ratesBDvarSpeExtTablesp) <- c('Sp.Y0', 'Sp.Y1', 'Sp.tc')
    row.names(ratesBDvarSpeExtTablesp) <- 'Rates'
    output$RateBDvarSpeExtModMLsp <- renderRHandsontable({rhandsontable(ratesBDvarSpeExtTablesp, readOnly = F)})
  }
  
  if (input$distBDvarSpeExtModMLsp == 'exp.t'){
    ratesBDvarSpeExtTablesp <- matrix(data = c(0.01,0.01),nrow = 1,ncol = 2)
    colnames(ratesBDvarSpeExtTablesp) <- c('Sp.l', 'Sp.a')
    row.names(ratesBDvarSpeExtTablesp) <- 'Rates'
    output$RateBDvarSpeExtModMLsp <- renderRHandsontable({rhandsontable(ratesBDvarSpeExtTablesp, readOnly = F)})
  }
  
  if (input$distBDvarSpeExtModMLsp == 'sigmoid.t'){
    ratesBDvarSpeExtTablesp <- matrix(data = c(0.01,0.01,0.01,0.01),nrow = 1,ncol = 4)
    colnames(ratesBDvarSpeExtTablesp) <- c('Sp.Y0', 'Sp.Y1','Sp.tmid', 'Sp.r')
    row.names(ratesBDvarSpeExtTablesp) <- 'Rates'
    output$RateBDvarSpeExtModMLsp <- renderRHandsontable({rhandsontable(ratesBDvarSpeExtTablesp, readOnly = F)})
  }
  
  if (input$distBDvarSpeExtModMLsp == 'spline.t'){
    
  }
  
  
  
})



# Extinction rates

observeEvent(input$distBDvarSpeExtModMLex, {
  
  #Extinction rates
  if (input$distBDvarSpeExtModMLex == 'linear.t'){
    ratesBDvarSpeExtTableex <- matrix(data = c(0.01,0.01),nrow = 1,ncol = 2)
    colnames(ratesBDvarSpeExtTableex) <- c('Ext.c', 'Ext.m')
    row.names(ratesBDvarSpeExtTableex) <- 'Rates'
    output$RateBDvarSpeExtModMLex <- renderRHandsontable({rhandsontable(ratesBDvarSpeExtTableex, readOnly = F)})
  }
  
  if (input$distBDvarSpeExtModMLex == 'stepf.t'){
    ratesBDvarSpeExtTableex <- matrix(data = c(0.01,0.01,0.01),nrow = 1,ncol = 3)
    colnames(ratesBDvarSpeExtTableex) <- c('Ext.Y0', 'Ext.Y1', 'Ext.tc')
    row.names(ratesBDvarSpeExtTableex) <- 'Rates'
    output$RateBDvarSpeExtModMLex <- renderRHandsontable({rhandsontable(ratesBDvarSpeExtTableex, readOnly = F)})
  }
  
  if (input$distBDvarSpeExtModMLex == 'exp.t'){
    ratesBDvarSpeExtTableex <- matrix(data = c(0.01,0.01),nrow = 1,ncol = 2)
    colnames(ratesBDvarSpeExtTableex) <- c('Ext.l', 'Ext.a')
    row.names(ratesBDvarSpeExtTableex) <- 'Rates'
    output$RateBDvarSpeExtModMLex <- renderRHandsontable({rhandsontable(ratesBDvarSpeExtTableex, readOnly = F)})
  }
  
  if (input$distBDvarSpeExtModMLex == 'sigmoid.t'){
    ratesBDvarSpeExtTableex <- matrix(data = c(0.01,0.01,0.01,0.01),nrow = 1,ncol = 4)
    colnames(ratesBDvarSpeExtTableex) <- c('Ext.Y0', 'Ext.Y1','Ext.tmid', 'Ext.r')
    row.names(ratesBDvarSpeExtTableex) <- 'Rates'
    output$RateBDvarSpeExtModMLex <- renderRHandsontable({rhandsontable(ratesBDvarExtTableex, readOnly = F)})
  }
  
  if (input$distBDvarSpeExtModMLex == 'spline.t'){
    
  }
  
  
})


observeEvent(input$addBDvarSpeExtModML,{
  
  DiverModML$iterBDvarSpeExt$rho <- as.numeric(input$fractBDvarSpeExtModML)
  DiverModML$iterBDvarSpeExt$Dist.Speciation <- input$distBDvarSpeExtModMLsp
  DiverModML$iterBDvarSpeExt$Rates.Speciation <- as.numeric(hot_to_r(input$RateBDvarSpeExtModMLsp))
  
  DiverModML$iterBDvarSpeExt$Dist.Extiction <- input$distBDvarSpeExtModMLex
  DiverModML$iterBDvarSpeExt$Rates.Extinction <- as.numeric(hot_to_r(input$RateBDvarSpeExtModMLex))
  
  DiverModML$BDvarSpeExtList$model <- DiverModML$iterBDvarSpeExt
  
  DiverModML$countBDvarSpeExt <- DiverModML$countBDvarSpeExt + 1
  
  names(DiverModML$BDvarSpeExtList)[DiverModML$countBDvarSpeExt] <- paste('BDvarSpeExt',DiverModML$countBDvarSpeExt, sep = '')
  
  DiverModML$modelList <- c(DiverModML$modelList,paste('BDvarSpeExt',DiverModML$countBDvarSpeExt, sep = ''))
  
  DiverModML$runObjModels$BDvarSpeExt <- DiverModML$BDvarSpeExtList
  
  
})


#Temporal object to print in info panel
# info:  models
observeEvent(input$addBDvarSpeExtModML, {
  
  
  DiverModML$iterObjectDiver <- DiverModML$runObjModels
  
  updateSelectInput(session = session,inputId = 'modelsFitModML',label = 'Models', choices = as.character(DiverModML$modelList),selected = as.character( DiverModML$modelList) )
})


# Diver-dependent model

observeEvent(input$addDiverDepentModML,{
  
  if (input$modelDiverModML == '1'){
    
    DiverModML$iterDiverDepentModML <- c("linear dependence in speciation rate with parameter K (= diversity where speciation = extinction)")
    
  }else if(input$modelDiverModML == '1.3'){
    
    DiverModML$iterDiverDepentModML <- c("linear dependence in speciation rate with parameter K' (= diversity where speciation = 0)")
    
  }else if(input$modelDiverModML == '1.4'){
    
    DiverModML$iterDiverDepentModML <- c(" positive diversity-dependence in speciation rate with parameter K' (= diversity where speciation rate reaches half its maximum); lambda = lambda0 * S/(S + K') where S is species richness")
    
  }else if(input$modelDiverModML == '1.5'){
    
    DiverModML$iterDiverDepentModML <- c("positive and negative dependence in speciation rate with parameter K' (= diversity where speciation = 0); lambda = lambda0 * S/K' * (1 - S/K') where S is species richness")
    
    
  }else if(input$modelDiverModML == '2'){
    
    DiverModML$iterDiverDepentModML <- c("exponential dependence in speciation rate with parameter K (= diversity where speciation = extinction)")
    
  }else if(input$modelDiverModML == '2.1'){
    
    DiverModML$iterDiverDepentModML <- c("variant of exponential dependence in speciation rate with offset at infinity")
    
  }else if(input$modelDiverModML == '2.2'){
    
    DiverModML$iterDiverDepentModML <- c("1/n dependence in speciation rate")
    
  }else if(input$modelDiverModML == '2.3'){
    
    DiverModML$iterDiverDepentModML <- c("exponential dependence in speciation rate with parameter x (= exponent)")
    
  }else if(input$modelDiverModML == '3'){
    
    DiverModML$iterDiverDepentModML <- c("linear dependence in extinction rate ")
    
  }else if(input$modelDiverModML == '4'){
    
    DiverModML$iterDiverDepentModML <- c("exponential dependence in extinction rate")

  }else if(input$modelDiverModML == '4.1'){
    
    DiverModML$iterDiverDepentModML <- c("variant of exponential dependence in extinction rate with offset at infinity")
    
  }else if(input$modelDiverModML == '4.2'){
    
    DiverModML$iterDiverDepentModML <- c("1/n dependence in extinction rate with offset at infinity ")

  }else if(input$modelDiverModML == '5'){

    DiverModML$iterDiverDepentModML <- c("linear dependence in speciation and extinction rate")

  }
  
  DiverModML$DiverDepentModMLList$model <- DiverModML$iterDiverDepentModML
  
  DiverModML$countDiverDepentModML <- DiverModML$countDiverDepentModML + 1
  
  names(DiverModML$DiverDepentModMLList)[DiverModML$countDiverDepentModML] <- paste('DiverDepentModel',DiverModML$countDiverDepentModML, sep = '')
  
  DiverModML$modelList <- c(DiverModML$modelList,paste('DiverDepentModel',DiverModML$countDiverDepentModML, sep = ''))
  
  DiverModML$runObjModels$DiverDepentModel <- DiverModML$DiverDepentModMLList
  
  
  
  
  
})


#Temporal object to print in info panel
# info:  models
observeEvent(input$addDiverDepentModML, {
  DiverModML$iterObjectDiver <- DiverModML$runObjModels
  
  updateSelectInput(session = session,inputId = 'modelsFitModML',label = 'Models', choices = as.character( DiverModML$modelList),selected = as.character( DiverModML$modelList))
})








### Running models
observeEvent(input$ModelRunModML,{


  # ##### Fitting Yule models


  yulemodels <- which(names(DiverModML$runObjModels$Yule) %in%  input$modelsFitModML)
 

  if (!length(yulemodels) == 0){
    if (input$optModML == 'optim'){
      # yule <-make.yule(tree = treeModML(), sampling.f = as.numeric(input$fractYuleModML), unresolved = NULL)
      # fityule <- find.mle(func = yule, x.init= as.numeric(input$BrateYuleModML), method='optim', control = list(optim.method= input$optimModML))
      #
      for (i in 1:length(yulemodels)){
        yule <-make.yule(tree = treeModML(), sampling.f = as.numeric(DiverModML$runObjModels$Yule[[yulemodels[i]]]$rho[1]), unresolved = NULL)
        fityule <- find.mle(func = yule, x.init= as.numeric(DiverModML$runObjModels$Yule[[yulemodels[i]]]$rate[1]),method='optim', control = list(optim.method= input$optimModML))

        DiverModML$iterYuleResult <- fityule

        DiverModML$ResultYuleList$result <- DiverModML$iterYuleResult

        DiverModML$countYuleResult <- DiverModML$countYuleResult + 1

        names(DiverModML$ResultYuleList)[DiverModML$countYuleResult] <- names(DiverModML$runObjModels$Yule)[yulemodels[i]]

        DiverModML$Result$YuleResults  <- DiverModML$ResultYuleList

        DiverModML$iterObjectDiver <- DiverModML$Result
        }
    } else if ( input$optModML == 'minqa'){
      # yule <-make.yule(tree = treeModML(), sampling.f = as.numeric(input$fractYuleModML), unresolved = NULL)
      # fityule <- find.mle(func = yule, x.init= as.numeric(input$BrateYuleModML), method='minqa', control = list(minqa.method= input$minqaModML))
      #
      for (i in 1:length(yulemodels)){
        yule <-make.yule(tree = treeModML(), sampling.f = as.numeric(DiverModML$runObjModels$Yule[[yulemodels[i]]]$rho[1]), unresolved = NULL)
        fityule <- find.mle(func = yule, x.init= as.numeric(DiverModML$runObjModels$Yule[[yulemodels[i]]]$rate[1]),method='minqa', control = list(minqa.method= input$minqaModML))

        DiverModML$iterYuleResult <- fityule

        DiverModML$ResultYuleList$result <- DiverModML$iterYuleResult

        DiverModML$countYuleResult <- DiverModML$countYuleResult + 1

        names(DiverModML$ResultYuleList)[DiverModML$countYuleResult] <- names(DiverModML$runObjModels$Yule)[yulemodels[i]]

        DiverModML$Result$YuleResults  <- DiverModML$ResultYuleList

        DiverModML$iterObjectDiver <- DiverModML$Result
        }
    } else {
      # yule <-make.yule(tree = treeModML(), sampling.f = as.numeric(input$fractYuleModML), unresolved = NULL)
      # fityule <- find.mle(func = yule, x.init= as.numeric(input$BrateYuleModML), method= input$optModML)
      #
      for (i in 1:length(yulemodels)){
        yule <-make.yule(tree = treeModML(), sampling.f = as.numeric(DiverModML$runObjModels$Yule[[yulemodels[i]]]$rho[1]), unresolved = NULL)
        fityule <- find.mle(func = yule, x.init= as.numeric(DiverModML$runObjModels$Yule[[yulemodels[i]]]$rate[1]),method= input$optModML)

        DiverModML$iterYuleResult <- fityule

        DiverModML$ResultYuleList$result <- DiverModML$iterYuleResult

        DiverModML$countYuleResult <- DiverModML$countYuleResult + 1

        names(DiverModML$ResultYuleList)[DiverModML$countYuleResult] <- names(DiverModML$runObjModels$Yule)[yulemodels[i]]

        DiverModML$Result$YuleResults  <- DiverModML$ResultYuleList

        DiverModML$iterObjectDiver <- DiverModML$Result
        }
    }

  }



  ##### Fitting BD models


  BDmodels <- which(names(DiverModML$runObjModels$BD) %in%  input$modelsFitModML)

  if (!length(BDmodels) == 0){
    if (input$optModML == 'optim'){
      for (i in 1:length(BDmodels)){
        BD <-make.bd(tree = treeModML(), sampling.f = as.numeric(DiverModML$runObjModels$BD[[BDmodels[i]]]$rho[1]), unresolved = NULL)
        fitBD <- find.mle(func = BD, x.init= as.numeric(c(DiverModML$runObjModels$BD[[BDmodels[i]]]$Rates[1],DiverModML$runObjModels$BD[[BDmodels[i]]]$Rates[2])),method='optim', control = list(optim.method= input$optimModML))

        DiverModML$iterBDResult <- fitBD

        DiverModML$ResultBDList$result <- DiverModML$iterBDResult

        DiverModML$countBDResult <- DiverModML$countBDResult + 1

        names(DiverModML$ResultBDList)[DiverModML$countBDResult] <- names(DiverModML$runObjModels$BD)[BDmodels[i]]

        DiverModML$Result$BDResults  <- DiverModML$ResultBDList

        DiverModML$iterObjectDiver <- DiverModML$Result
      }
    } else if ( input$optModML == 'minqa'){
      # yule <-make.yule(tree = treeModML(), sampling.f = as.numeric(input$fractYuleModML), unresolved = NULL)
      # fityule <- find.mle(func = yule, x.init= as.numeric(input$BrateYuleModML), method='minqa', control = list(minqa.method= input$minqaModML))
      #
      for (i in 1:length(BDmodels)){
        BD <-make.bd(tree = treeModML(), sampling.f = as.numeric(DiverModML$runObjModels$BD[[BDmodels[i]]]$rho[1]), unresolved = NULL)
        fitBD <- find.mle(func = BD, x.init= as.numeric(c(DiverModML$runObjModels$BD[[BDmodels[i]]]$Rates[1],DiverModML$runObjModels$BD[[BDmodels[i]]]$Rates[2])), method='minqa', control = list(minqa.method= input$minqaModML))

        DiverModML$iterBDResult <- fitBD

        DiverModML$ResultBDList$result <- DiverModML$iterBDResult

        DiverModML$countBDResult <- DiverModML$countBDResult + 1

        names(DiverModML$ResultBDList)[DiverModML$countBDResult] <- names(DiverModML$runObjModels$BD)[BDmodels[i]]

        DiverModML$Result$BDResults  <- DiverModML$ResultBDList

        DiverModML$iterObjectDiver <- DiverModML$Result
      }
    } else {
      # yule <-make.yule(tree = treeModML(), sampling.f = as.numeric(input$fractYuleModML), unresolved = NULL)
      # fityule <- find.mle(func = yule, x.init= as.numeric(input$BrateYuleModML), method= input$optModML)
      #
      for (i in 1:length(BDmodels)){
        BD <-make.bd(tree = treeModML(), sampling.f = as.numeric(DiverModML$runObjModels$BD[[BDmodels[i]]]$rho[1]), unresolved = NULL)
        fitBD <- find.mle(func = BD, x.init= as.numeric(c(DiverModML$runObjModels$BD[[BDmodels[i]]]$Rates[1],DiverModML$runObjModels$BD[[BDmodels[i]]]$Rates[2])),method= input$optModML)

        DiverModML$iterBDResult <- fitBD

        DiverModML$ResultBDList$result <- DiverModML$iterBDResult

        DiverModML$countBDResult <- DiverModML$countBDResult + 1

        names(DiverModML$ResultBDList)[DiverModML$countBDResult] <- names(DiverModML$runObjModels$BD)[BDmodels[i]]

        DiverModML$Result$BDResults  <- DiverModML$ResultBDList

        DiverModML$iterObjectDiver <- DiverModML$Result
      }
    }

  }



  #### Fitting BDvarSp models


  BDvarSpemodels <- which(names(DiverModML$runObjModels$BDvarSpe) %in%  input$modelsFitModML)

  if (!length(BDvarSpemodels) == 0){

    if (input$optModML == 'optim'){
      for (i in 1:length(BDvarSpemodels)){
        BDvarSpe <-make.bd.t(tree = treeModML(),
                             sampling.f = as.numeric(DiverModML$runObjModels$BDvarSpe[[BDvarSpemodels[i]]]$rho[1]),
                             unresolved = NULL,
                             functions = c(DiverModML$runObjModels$BDvarSpe[[BDvarSpemodels[i]]]$Distribution[1], 'constant.t'))


        fitBDvarSpe <- find.mle(func = BDvarSpe,
                                x.init= as.numeric(c(DiverModML$runObjModels$BDvarSpe[[BDvarSpemodels[i]]]$Rates)),method='optim', control = list(optim.method= input$optimModML))

        DiverModML$iterBDvarSpeResult <- fitBDvarSpe

        DiverModML$ResultBDvarSpeList$result <- DiverModML$iterBDvarSpeResult

        DiverModML$countBDvarSpeResult <- DiverModML$countBDvarSpeResult + 1

        names(DiverModML$ResultBDvarSpeList)[DiverModML$countBDvarSpeResult] <- names(DiverModML$runObjModels$BDvarSpe)[BDvarSpemodels[i]]

        DiverModML$Result$BDvarSpeResults  <- DiverModML$ResultBDvarSpeList

        DiverModML$iterObjectDiver <- DiverModML$Result
      }
    } else if ( input$optModML == 'minqa'){
      # yule <-make.yule(tree = treeModML(), sampling.f = as.numeric(input$fractYuleModML), unresolved = NULL)
      # fityule <- find.mle(func = yule, x.init= as.numeric(input$BrateYuleModML), method='minqa', control = list(minqa.method= input$minqaModML))
      #
      for (i in 1:length(BDvarSpemodels)){
        BDvarSpe <-make.bd.t(tree = treeModML(),
                             sampling.f = as.numeric(DiverModML$runObjModels$BDvarSpe[[BDvarSpemodels[i]]]$rho[1]),
                             unresolved = NULL,
                             functions = c(DiverModML$runObjModels$BDvarSpe[[BDvarSpemodels[i]]]$Distribution[1], 'constant.t'))


        fitBDvarSpe <- find.mle(func = BDvarSpe,
                                x.init= as.numeric(c(DiverModML$runObjModels$BDvarSpe[[BDvarSpemodels[i]]]$Rates)),method='minqa', control = list(minqa.method= input$minqaModML))

        DiverModML$iterBDvarSpeResult <- fitBDvarSpe

        DiverModML$ResultBDvarSpeList$result <- DiverModML$iterBDvarSpeResult

        DiverModML$countBDvarSpeResult <- DiverModML$countBDvarSpeResult + 1

        names(DiverModML$ResultBDvarSpeList)[DiverModML$countBDvarSpeResult] <- names(DiverModML$runObjModels$BDvarSpe)[BDvarSpemodels[i]]

        DiverModML$Result$BDvarSpeResults  <- DiverModML$ResultBDvarSpeList

        DiverModML$iterObjectDiver <- DiverModML$Result
      }
    } else {
      # yule <-make.yule(tree = treeModML(), sampling.f = as.numeric(input$fractYuleModML), unresolved = NULL)
      # fityule <- find.mle(func = yule, x.init= as.numeric(input$BrateYuleModML), method= input$optModML)
      #
      for (i in 1:length(BDvarSpemodels)){
        BDvarSpe <-make.bd.t(tree = treeModML(),
                             sampling.f = as.numeric(DiverModML$runObjModels$BDvarSpe[[BDvarSpemodels[i]]]$rho[1]),
                             unresolved = NULL,
                             functions = c(DiverModML$runObjModels$BDvarSpe[[BDvarSpemodels[i]]]$Distribution[1], 'constant.t'))


        fitBDvarSpe <- find.mle(func = BDvarSpe,
                                x.init= as.numeric(c(DiverModML$runObjModels$BDvarSpe[[BDvarSpemodels[i]]]$Rates)),method= input$optModML)

              DiverModML$iterBDvarSpeResult <- fitBDvarSpe

              DiverModML$ResultBDvarSpeList$result <- DiverModML$iterBDvarSpeResult

              DiverModML$countBDvarSpeResult <- DiverModML$countBDvarSpeResult + 1

              names(DiverModML$ResultBDvarSpeList)[DiverModML$countBDvarSpeResult] <- names(DiverModML$runObjModels$BDvarSpe)[BDvarSpemodels[i]]

              DiverModML$Result$BDvarSpeResults  <- DiverModML$ResultBDvarSpeList

              DiverModML$iterObjectDiver <- DiverModML$Result
      }
    }

  }



  #### Fitting BDvarExt models


  BDvarExtmodels <- which(names(DiverModML$runObjModels$BDvarExt) %in%  input$modelsFitModML)

  if (!length(BDvarExtmodels) == 0){

    if (input$optModML == 'optim'){
      for (i in 1:length(BDvarExtmodels)){
        BDvarExt <-make.bd.t(tree = treeModML(),
                             sampling.f = as.numeric(DiverModML$runObjModels$BDvarExt[[BDvarExtmodels[i]]]$rho[1]),
                             unresolved = NULL,
                             functions = c('constant.t', DiverModML$runObjModels$BDvarExt[[BDvarExtmodels[i]]]$Distribution[1]))


        fitBDvarExt <- find.mle(func = BDvarExt,
                                x.init= as.numeric(c(DiverModML$runObjModels$BDvarExt[[BDvarExtmodels[i]]]$Rates)),method='optim', control = list(optim.method= input$optimModML))

        DiverModML$iterBDvarExtResult <- fitBDvarExt

        DiverModML$ResultBDvarExtList$result <- DiverModML$iterBDvarExtResult

        DiverModML$countBDvarExtResult <- DiverModML$countBDvarExtResult + 1

        names(DiverModML$ResultBDvarExtList)[DiverModML$countBDvarExtResult] <- names(DiverModML$runObjModels$BDvarExt)[BDvarExtmodels[i]]

        DiverModML$Result$BDvarExtResults  <- DiverModML$ResultBDvarExtList

        DiverModML$iterObjectDiver <- DiverModML$Result
      }
    } else if ( input$optModML == 'minqa'){
      # yule <-make.yule(tree = treeModML(), sampling.f = as.numeric(input$fractYuleModML), unresolved = NULL)
      # fityule <- find.mle(func = yule, x.init= as.numeric(input$BrateYuleModML), method='minqa', control = list(minqa.method= input$minqaModML))
      #
      for (i in 1:length(BDvarExtmodels)){
        BDvarExt <-make.bd.t(tree = treeModML(),
                             sampling.f = as.numeric(DiverModML$runObjModels$BDvarExt[[BDvarExtmodels[i]]]$rho[1]),
                             unresolved = NULL,
                             functions = c('constant.t', DiverModML$runObjModels$BDvarExt[[BDvarExtmodels[i]]]$Distribution[1]))


        fitBDvarExt <- find.mle(func = BDvarExt,
                                x.init= as.numeric(c(DiverModML$runObjModels$BDvarExt[[BDvarExtmodels[i]]]$Rates)),method='minqa', control = list(minqa.method= input$minqaModML))

        DiverModML$iterBDvarExtResult <- fitBDvarExt

        DiverModML$ResultBDvarExtList$result <- DiverModML$iterBDvarExtResult

        DiverModML$countBDvarExtResult <- DiverModML$countBDvarExtResult + 1

        names(DiverModML$ResultBDvarExtList)[DiverModML$countBDvarExtResult] <- names(DiverModML$runObjModels$BDvarExt)[BDvarExtmodels[i]]

        DiverModML$Result$BDvarExtResults  <- DiverModML$ResultBDvarExtList

        DiverModML$iterObjectDiver <- DiverModML$Result
      }
    } else {
      # yule <-make.yule(tree = treeModML(), sampling.f = as.numeric(input$fractYuleModML), unresolved = NULL)
      # fityule <- find.mle(func = yule, x.init= as.numeric(input$BrateYuleModML), method= input$optModML)
      #
      for (i in 1:length(BDvarExtmodels)){
        BDvarExt <-make.bd.t(tree = treeModML(),
                             sampling.f = as.numeric(DiverModML$runObjModels$BDvarExt[[BDvarExtmodels[i]]]$rho[1]),
                             unresolved = NULL,
                             functions = c('constant.t', DiverModML$runObjModels$BDvarExt[[BDvarExtmodels[i]]]$Distribution[1]))


        fitBDvarExt <- find.mle(func = BDvarExt,
                                x.init= as.numeric(c(DiverModML$runObjModels$BDvarExt[[BDvarExtmodels[i]]]$Rates)),method= input$optModML)

        DiverModML$iterBDvarExtResult <- fitBDvarExt

        DiverModML$ResultBDvarExtList$result <- DiverModML$iterBDvarExtResult

        DiverModML$countBDvarExtResult <- DiverModML$countBDvarExtResult + 1

        names(DiverModML$ResultBDvarExtList)[DiverModML$countBDvarExtResult] <- names(DiverModML$runObjModels$BDvarExt)[BDvarExtmodels[i]]

        DiverModML$Result$BDvarExtResults  <- DiverModML$ResultBDvarExtList

        DiverModML$iterObjectDiver <- DiverModML$Result
      }
    }

  }



  #### Fitting BDvarSpeExt models


  BDvarSpeExtmodels <- which(names(DiverModML$runObjModels$BDvarSpeExt) %in%  input$modelsFitModML)

  if (!length(BDvarSpeExtmodels) == 0){

    if (input$optModML == 'optim'){
      for (i in 1:length(BDvarSpeExtmodels)){
        BDvarSpeExt <-make.bd.t(tree = treeModML(),
                             sampling.f = as.numeric(DiverModML$runObjModels$BDvarSpeExt[[BDvarSpeExtmodels[i]]]$rho[1]),
                             unresolved = NULL,
                             functions = c(DiverModML$runObjModels$BDvarSpeExt[[BDvarSpeExtmodels[i]]]$Dist.Speciation[1], DiverModML$runObjModels$BDvarSpeExt[[BDvarSpeExtmodels[i]]]$Dist.Extinction[1]))


        fitBDvarSpeExt <- find.mle(func = BDvarSpeExt,
                                x.init= as.numeric(c(DiverModML$runObjModels$BDvarSpeExt[[BDvarSpeExtmodels[i]]]$Rates.Speciation,DiverModML$runObjModels$BDvarSpeExt[[BDvarSpeExtmodels[i]]]$Rates.Extinction)),method='optim', control = list(optim.method= input$optimModML))

        DiverModML$iterBDvarSpeExtResult <- fitBDvarSpeExt

        DiverModML$ResultBDvarSpeExtList$result <- DiverModML$iterBDvarSpeExtResult

        DiverModML$countBDvarSpeExtResult <- DiverModML$countBDvarSpeExtResult + 1

        names(DiverModML$ResultBDvarSpeExtList)[DiverModML$countBDvarSpeExtResult] <- names(DiverModML$runObjModels$BDvarSpeExt)[BDvarSpeExtmodels[i]]

        DiverModML$Result$BDvarSpeExtResults  <- DiverModML$ResultBDvarSpeExtList

        DiverModML$iterObjectDiver <- DiverModML$Result
      }
    } else if ( input$optModML == 'minqa'){
      # yule <-make.yule(tree = treeModML(), sampling.f = as.numeric(input$fractYuleModML), unresolved = NULL)
      # fityule <- find.mle(func = yule, x.init= as.numeric(input$BrateYuleModML), method='minqa', control = list(minqa.method= input$minqaModML))
      #
      for (i in 1:length(BDvarSpeExtmodels)){
        BDvarSpeExt <-make.bd.t(tree = treeModML(),
                             sampling.f = as.numeric(DiverModML$runObjModels$BDvarSpeExt[[BDvarSpeExtmodels[i]]]$rho[1]),
                             unresolved = NULL,
                             functions = c(DiverModML$runObjModels$BDvarSpeExt[[BDvarSpeExtmodels[i]]]$Dist.Speciation[1], DiverModML$runObjModels$BDvarSpeExt[[BDvarSpeExtmodels[i]]]$Dist.Extinction[1]))


        fitBDvarSpeExt <- find.mle(func = BDvarSpeExt,
                                x.init= as.numeric(c(DiverModML$runObjModels$BDvarSpeExt[[BDvarSpeExtmodels[i]]]$Rates.Speciation,DiverModML$runObjModels$BDvarSpeExt[[BDvarSpeExtmodels[i]]]$Rates.Extinction)),method='minqa', control = list(minqa.method= input$minqaModML))

        DiverModML$iterBDvarSpeExtResult <- fitBDvarSpeExt

        DiverModML$ResultBDvarSpeExtList$result <- DiverModML$iterBDvarSpeExtResult

        DiverModML$countBDvarSpeExtResult <- DiverModML$countBDvarSpeExtResult + 1

        names(DiverModML$ResultBDvarSpeExtList)[DiverModML$countBDvarSpeExtResult] <- names(DiverModML$runObjModels$BDvarSpeExt)[BDvarSpeExtmodels[i]]

        DiverModML$Result$BDvarSpeExtResults  <- DiverModML$ResultBDvarSpeExtList

        DiverModML$iterObjectDiver <- DiverModML$Result
      }
    } else {
      # yule <-make.yule(tree = treeModML(), sampling.f = as.numeric(input$fractYuleModML), unresolved = NULL)
      # fityule <- find.mle(func = yule, x.init= as.numeric(input$BrateYuleModML), method= input$optModML)
      #
      for (i in 1:length(BDvarSpeExtmodels)){
        BDvarSpeExt <-make.bd.t(tree = treeModML(),
                             sampling.f = as.numeric(DiverModML$runObjModels$BDvarSpeExt[[BDvarSpeExtmodels[i]]]$rho[1]),
                             unresolved = NULL,
                             functions = c(DiverModML$runObjModels$BDvarSpeExt[[BDvarSpeExtmodels[i]]]$Dist.Speciation[1], DiverModML$runObjModels$BDvarSpeExt[[BDvarSpeExtmodels[i]]]$Dist.Extinction[1]))


        fitBDvarSpeExt <- find.mle(func = BDvarSpeExt,
                                x.init= as.numeric(c(DiverModML$runObjModels$BDvarSpeExt[[BDvarSpeExtmodels[i]]]$Rates.Speciation,DiverModML$runObjModels$BDvarSpeExt[[BDvarSpeExtmodels[i]]]$Rates.Extinction)),method= input$optModML)

        DiverModML$iterBDvarSpeExtResult <- fitBDvarSpeExt

        DiverModML$ResultBDvarSpeExtList$result <- DiverModML$iterBDvarSpeExtResult

        DiverModML$countBDvarSpeExtResult <- DiverModML$countBDvarSpeExtResult + 1

        names(DiverModML$ResultBDvarSpeExtList)[DiverModML$countBDvarSpeExtResult] <- names(DiverModML$runObjModels$BDvarSpeExt)[BDvarSpeExtmodels[i]]

        DiverModML$Result$BDvarSpeExtResults  <- DiverModML$ResultBDvarSpeExtList

        DiverModML$iterObjectDiver <- DiverModML$Result
      }
    }

  }

  #### Fit DiverDepentModel

  DiverDepentModelmodels <- which(names(DiverModML$runObjModels$DiverDepentModel) %in%  input$modelsFitModML)

  if (!length(DiverDepentModelmodels) == 0){

    branchingTime <- branching.times(treeModML())

    for (i in 1:length(DiverDepentModelmodels)){

      bd_res_ddd <- bd_ML(brts = branchingTime, missnumspec = as.numeric(input$missnumDiverModML), cond = as.numeric(input$modelDiverModML), btorph = 0,optimmethod = input$optModMLDDD)
      
      DiverModML$iterDiverDepentModelResult <- bd_res_ddd
       
       DiverModML$ResultDiverDepentModelList$result <- DiverModML$iterDiverDepentModelResult
       
       DiverModML$countDiverDepentModelResult <- DiverModML$countDiverDepentModelResult + 1
       
      names(DiverModML$ResultDiverDepentModelList)[DiverModML$countDiverDepentModelResult] <- names(DiverModML$runObjModels$DiverDepentModel)[DiverDepentModelmodels[i]]
      
      DiverModML$Result$DiverDepentModelResults  <- DiverModML$ResultDiverDepentModelList 
      
      DiverModML$iterObjectDiver <- DiverModML$Result
    }

  #DiverModML$runObjModels$DiverDepentModel


  }
})







# #Temporal object to print in info panel
# # info:  models
# observeEvent(input$ModelRunModML, {
#   
#   
#   DiverModML$iterObjectDiver <- DiverModML$runObjModels
#   
# })

