##############################################################################
# Diversity: Models ML
##############################################################################

#Temporal objects
#
DiverModML <- reactiveValues()
DiverModML$iterObjectDiver <- NULL


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
