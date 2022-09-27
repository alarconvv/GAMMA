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

DiverModML$countyule <- 0
DiverModML$countBD <- 0
DiverModML$countBDvarSpe <- 0

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
})


#Temporal object to print in info panel
# info:  models
observeEvent(input$addBDContModML, {
  DiverModML$iterObjectDiver <- DiverModML$runObjModels
})



# BDvarSp Model

observeEvent(input$distBDvarSpeModML != 'select',{
  DiverModML$runObjModels$BDvarSpe <- NULL
  
  if (input$distBDvarSpeModML == 'linear.t'){
    ratesBDvarSpeTable <- matrix(data = c(0.1,0.01,0.01),nrow = 1,ncol = 3)
    colnames(ratesBDvarSpeTable) <- c('Sp.c', 'Sp.m','Ext')
    row.names(ratesBDvarSpeTable) <- 'Rates'
    
  }

  if (input$distBDvarSpeModML == 'spline.t'){
    ratesBDvarSpeTable <- matrix(data = c(0.1,0.01,0.01),nrow = 1,ncol = 3)
    colnames(ratesBDvarSpeTable) <- c('Sp.Y0', 'Sp.Y1','Ext')
    row.names(ratesBDvarSpeTable) <- 'Rates'
    output$RateBDvarSpeModML <- renderRHandsontable({rhandsontable(ratesBBDvarSpeTable, readOnly = F)})
    
  }
  
  if (input$distBDvarSpeModML == 'exp.t'){
    ratesBDvarSpeTable <- matrix(data = c(0.1,0.01,0.01),nrow = 1,ncol = 3)
    colnames(ratesBDvarSpeTable) <- c('Sp.l', 'Sp.a','Ext')
    row.names(ratesBDvarSpeTable) <- 'Rates'
    output$RateBDvarSpeModML <- renderRHandsontable({rhandsontable(ratesBBDvarSpeTable, readOnly = F)})
    
  }
  if (input$distBDvarSpeModML == 'sigmoid.t'){
    ratesBDvarSpeTable <- matrix(data = c(0.1,0.01,0.01,0.01,0.01),nrow = 1,ncol = 5)
    colnames(ratesBDvarSpeTable) <- c('Sp.Y0', 'Sp.Y1','Sp.tmid', 'Sp.r','Ext')
    row.names(ratesBDvarSpeTable) <- 'Rates'
    output$RateBDvarSpeModML <- renderRHandsontable({rhandsontable(ratesBBDvarSpeTable, readOnly = F)})
    
  }
  
  if (input$distBDvarSpeModML == 'stepf.t'){
    
  }
  
  output$RateBDvarSpeModML <- renderUI({
    #no funcional
    rHandsontableOutput(outputId = "RateBDvarSpeModML")
  })

})


observeEvent(input$addBDvarSpeModML,{
  DiverModML$iterBDvarSpe$rho <- as.numeric(input$fractBDContModML)
  DiverModML$iterBDvarSpe$Rates <- as.numeric(hot_to_r(input$BDrateModML))
  DiverModML$BDvarSpeList$model <- DiverModML$iterBDvarSpe
  
  DiverModML$countBDvarSpe <- DiverModML$countBDvarSpe + 1
  
  names(DiverModML$BDvarSpeList)[DiverModML$countBDvarSpe] <- paste('BDvarSpe',DiverModML$countBDvarSpe, sep = '')
  
  DiverModML$runObjModels$BDvarSpe <- DiverModML$BDvarSpeList
})


#Temporal object to print in info panel
# info:  models
observeEvent(input$addBDContModML, {
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
