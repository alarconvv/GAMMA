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

DiverModML$countyule <- 0
DiverModML$countBD <- 0

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
  DiverModML$iterBD$Brate <- as.numeric(hot_to_r(input$BDrateModML)[1,1])
  DiverModML$iterBD$Drate <- as.numeric(hot_to_r(input$BDrateModML)[1,2])
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

observeEvent(input$BDContModML,{
  DiverModML$runObjModels$BD <- NULL
  
  
  
  ratesBDTable <- matrix(data = c(0.1,0.1),nrow = 1,ncol = 2)
  colnames(ratesBDTable) <- c('Sp', 'Ext')
  row.names(ratesBDTable) <- 'Rates'
  
  output$BDrateModML <- renderRHandsontable({rhandsontable(ratesBDTable, readOnly = F)})
})

observeEvent(input$addBDContModML,{
  
  DiverModML$iterBD$rho <- as.numeric(input$fractBDContModML)
  DiverModML$iterBD$Brate <- as.numeric(hot_to_r(input$BDrateModML)[1,1])
  DiverModML$iterBD$Drate <- as.numeric(hot_to_r(input$BDrateModML)[1,2])
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
