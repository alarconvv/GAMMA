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

DiverModML$countyule <- 0
DiverModML$countBD <- 0
DiverModML$countBDvarSpe <- 0
DiverModML$countBDvarExt <- 0
DiverModML$countBDvarSpeExt <- 0
DiverModML$countDiverDepentModML <- 0

#Render print in Info panel: Models: ML
#
output$infoPanelDiverModML <- renderPrint( {
  print(DiverModML$iterObjectDiver)
})


## Tree 

treeModML <- reactive(treeInputDiver())



#plot


#Plot tree
#
heightDiverModML <- reactive(input$PlotHeightDiverModML[1])
widthDiverModML <- reactive(input$PlotWidthDiverModML[1])

output$PhyloPlotDiver5 <- renderPlot( height = heightDiverModML  , width = widthDiverModML,{
  req(treeModML())
  
  
  plot.phylo(treeInputDiver(), show.tip.label = T,
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
    
    DiverModML$runObjModels$Yule <- DiverModML$YuleList
    
    updateSelectInput(session = session,inputId = 'modelsFitModML',label = 'Models', choices = c(input$modelsFitModML,paste('Yule',DiverModML$countyule, sep = '')))
})

#Temporal object to print in info panel
# info:  models
observeEvent(input$addYuleModML, {
  
  DiverModML$iterObjectDiver <- DiverModML$runObjModels
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
  
  DiverModML$runObjModels$BD <- DiverModML$BDList
  
  
  
  updateSelectInput(session = session,inputId = 'modelsFitModML',label = 'Models', choices = c(input$modelsFitModML,paste('BD',DiverModML$countBD, sep = '')))
})


#Temporal object to print in info panel
# info:  models
observeEvent(input$addBDContModML, {
  DiverModML$iterObjectDiver <- DiverModML$runObjModels
})



# BDvarSp Model

observeEvent(input$distBDvarSpeModML,{
  DiverModML$runObjModels$BDvarSpe <- NULL
  
  if (input$distBDvarSpeModML == 'linear.t'){
    ratesBDvarSpeTable <- matrix(data = c(0.1,0.01,0.01),nrow = 1,ncol = 3)
    colnames(ratesBDvarSpeTable) <- c('Sp.c', 'Sp.m','Ext')
    row.names(ratesBDvarSpeTable) <- 'Rates'
    output$RateBDvarSpeModML <- renderRHandsontable({rhandsontable(ratesBDvarSpeTable, readOnly = F)})
  }

  if (input$distBDvarSpeModML == 'stepf.t'){
    ratesBDvarSpeTable <- matrix(data = c(0.1,0.01,0.01),nrow = 1,ncol = 3)
    colnames(ratesBDvarSpeTable) <- c('Sp.Y0', 'Sp.Y1','Ext')
    row.names(ratesBDvarSpeTable) <- 'Rates'
    output$RateBDvarSpeModML <- renderRHandsontable({rhandsontable(ratesBDvarSpeTable, readOnly = F)})
  }
  
  if (input$distBDvarSpeModML == 'exp.t'){
    ratesBDvarSpeTable <- matrix(data = c(0.1,0.01,0.01),nrow = 1,ncol = 3)
    colnames(ratesBDvarSpeTable) <- c('Sp.l', 'Sp.a','Ext')
    row.names(ratesBDvarSpeTable) <- 'Rates'
    output$RateBDvarSpeModML <- renderRHandsontable({rhandsontable(ratesBDvarSpeTable, readOnly = F)})
  }
  
  if (input$distBDvarSpeModML == 'sigmoid.t'){
    ratesBDvarSpeTable <- matrix(data = c(0.1,0.01,0.01,0.01,0.01),nrow = 1,ncol = 5)
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
  
  DiverModML$runObjModels$BDvarSpe <- DiverModML$BDvarSpeList
  
  
  
  updateSelectInput(session = session,inputId = 'modelsFitModML',label = 'Models', choices = c(input$modelsFitModML,paste('BDvarSpe',DiverModML$countBDvarSpe, sep = '')))
})


#Temporal object to print in info panel
# info:  models
observeEvent(input$addBDvarSpeModML, {
  DiverModML$iterObjectDiver <- DiverModML$runObjModels
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
    ratesBDvarExtTable <- matrix(data = c(0.1,0.01,0.01),nrow = 1,ncol = 3)
    colnames(ratesBDvarExtTable) <- c('Sp','Ext.Y0', 'Ext.Y1')
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
  
  DiverModML$runObjModels$BDvarExt <- DiverModML$BDvarExtList
  
  
  updateSelectInput(session = session,inputId = 'modelsFitModML',label = 'Models', choices = c(input$modelsFitModML,paste('BDvarExt',DiverModML$countBDvarExt, sep = '')))
})


#Temporal object to print in info panel
# info:  models
observeEvent(input$addBDvarExtModML, {
  DiverModML$iterObjectDiver <- DiverModML$runObjModels
})


# BDvarSpeExt Model

observeEvent(input$distBDvarSpeExtModMLsp,{
  DiverModML$runObjModels$BDvarSpeExt <- NULL
  
  #Speciation rate
  if (input$distBDvarSpeExtModMLsp == 'linear.t'){
    ratesBDvarSpeExtTablesp <- matrix(data = c(0.1,0.01,0.01),nrow = 1,ncol = 3)
    colnames(ratesBDvarSpeExtTablesp) <- c('Sp','Ext.c', 'Ext.m')
    row.names(ratesBDvarSpeExtTablesp) <- 'Rates'
    output$RateBDvarSpeExtModMLsp <- renderRHandsontable({rhandsontable(ratesBDvarSpeExtTablesp, readOnly = F)})
  }
  
  if (input$distBDvarSpeExtModMLsp == 'stepf.t'){
    ratesBDvarSpeExtTablesp <- matrix(data = c(0.1,0.01,0.01),nrow = 1,ncol = 3)
    colnames(ratesBDvarSpeExtTablesp) <- c('Sp','Ext.Y0', 'Ext.Y1')
    row.names(ratesBDvarSpeExtTablesp) <- 'Rates'
    output$RateBDvarSpeExtModMLsp <- renderRHandsontable({rhandsontable(ratesBDvarSpeExtTablesp, readOnly = F)})
  }
  
  if (input$distBDvarSpeExtModMLsp == 'exp.t'){
    ratesBDvarSpeExtTablesp <- matrix(data = c(0.1,0.01,0.01),nrow = 1,ncol = 3)
    colnames(ratesBDvarSpeExtTablesp) <- c('Sp','Ext.l', 'Ext.a')
    row.names(ratesBDvarSpeExtTablesp) <- 'Rates'
    output$RateBDvarSpeExtModMLsp <- renderRHandsontable({rhandsontable(ratesBDvarSpeExtTablesp, readOnly = F)})
  }
  
  if (input$distBDvarSpeExtModMLsp == 'sigmoid.t'){
    ratesBDvarSpeExtTablesp <- matrix(data = c(0.1,0.01,0.01,0.01,0.01),nrow = 1,ncol = 5)
    colnames(ratesBDvarSpeExtTablesp) <- c('Sp','Ext.Y0', 'Ext.Y1','Ext.tmid', 'Ext.r')
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
    ratesBDvarSpeExtTableex <- matrix(data = c(0.1,0.01,0.01),nrow = 1,ncol = 3)
    colnames(ratesBDvarSpeExtTableex) <- c('Sp','Ext.c', 'Ext.m')
    row.names(ratesBDvarSpeExtTableex) <- 'Rates'
    output$RateBDvarSpeExtModMLex <- renderRHandsontable({rhandsontable(ratesBDvarSpeExtTableex, readOnly = F)})
  }
  
  if (input$distBDvarSpeExtModMLex == 'stepf.t'){
    ratesBDvarSpeExtTableex <- matrix(data = c(0.1,0.01,0.01),nrow = 1,ncol = 3)
    colnames(ratesBDvarSpeExtTableex) <- c('Sp','Ext.Y0', 'Ext.Y1')
    row.names(ratesBDvarSpeExtTableex) <- 'Rates'
    output$RateBDvarSpeExtModMLex <- renderRHandsontable({rhandsontable(ratesBDvarSpeExtTableex, readOnly = F)})
  }
  
  if (input$distBDvarSpeExtModMLex == 'exp.t'){
    ratesBDvarSpeExtTableex <- matrix(data = c(0.1,0.01,0.01),nrow = 1,ncol = 3)
    colnames(ratesBDvarSpeExtTableex) <- c('Sp','Ext.l', 'Ext.a')
    row.names(ratesBDvarSpeExtTableex) <- 'Rates'
    output$RateBDvarSpeExtModMLex <- renderRHandsontable({rhandsontable(ratesBDvarSpeExtTableex, readOnly = F)})
  }
  
  if (input$distBDvarSpeExtModMLex == 'sigmoid.t'){
    ratesBDvarSpeExtTableex <- matrix(data = c(0.1,0.01,0.01,0.01,0.01),nrow = 1,ncol = 5)
    colnames(ratesBDvarSpeExtTableex) <- c('Sp','Ext.Y0', 'Ext.Y1','Ext.tmid', 'Ext.r')
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
  
  DiverModML$runObjModels$BDvarSpeExt <- DiverModML$BDvarSpeExtList
  
  
  updateSelectInput(session = session,inputId = 'modelsFitModML',label = 'Models', choices = c(input$modelsFitModML,paste('BDvarSpeExt',DiverModML$countBDvarSpeExt, sep = '')))
})


#Temporal object to print in info panel
# info:  models
observeEvent(input$addBDvarSpeExtModML, {
  
  
  DiverModML$iterObjectDiver <- DiverModML$runObjModels
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
  
  DiverModML$runObjModels$DiverDepentModel <- DiverModML$DiverDepentModMLList
  
  updateSelectInput(session = session,inputId = 'modelsFitModML',label = 'Models', choices = c(input$modelsFitModML,paste('DiverDepentModel',DiverModML$countDiverDepentModML, sep = '')))
  
  
  
})




#Temporal object to print in info panel
# info:  models
observeEvent(input$addDiverDepentModML, {
  DiverModML$iterObjectDiver <- DiverModML$runObjModels
})





### models
observeEvent(input$ModelRunModML,{
  
 
  if (input$yuleModML){
    #unresolve
    if (input$optModML == 'optim'){
      yule <-make.yule(tree = treeModML(), sampling.f = as.numeric(input$fractYuleModML), unresolved = NULL)
      fityule <- find.mle(func = yule, x.init= as.numeric(input$BrateYuleModML), method='optim', control = list(optim.method= input$optimModML))
    } else if ( input$optModML == 'minqa'){
      yule <-make.yule(tree = treeModML(), sampling.f = as.numeric(input$fractYuleModML), unresolved = NULL)
      fityule <- find.mle(func = yule, x.init= as.numeric(input$BrateYuleModML), method='minqa', control = list(minqa.method= input$minqaModML))
    } else {
      yule <-make.yule(tree = treeModML(), sampling.f = as.numeric(input$fractYuleModML), unresolved = NULL)
      fityule <- find.mle(func = yule, x.init= as.numeric(input$BrateYuleModML), method= input$optModML)
    }
  }
  
  
  
  
})
