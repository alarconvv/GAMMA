##############################################################################
# Diversity: Lineage through the time
##############################################################################

#Temporal objects
#
DiverLtt <- reactiveValues()
DiverLtt$iterObjectDiver <- NULL


#Render print in Info panel: Data panel
#
output$infoPanelDiverLtt <- renderPrint( {
  print(DiverLtt$iterObjectDiver)
})


#Get tree from DT

treeDiverLTT <- reactive({
 
  validate(need(try(treeInputDiver5()),"Please, your tree must be ultrametric and binary ")
  )
  treeInputDiver5()
}
)







#Plot tree
#
heightDiverLtt <- reactive(input$PlotHeightDiverLtt[1])
widthDiverLtt <- reactive(input$PlotWidthDiverLtt[1])

output$PhyloPlotDiver2 <- renderPlot( height = heightDiverLtt  , width = widthDiverLtt,{
  req(treeDiverLTT())
  
  
  plot.phylo(treeDiverLTT(), show.tip.label = T,
                         cex = input$tipSizeDiverLtt[1], use.edge.length = T,
                         edge.width = 0.8,edge.color = 'grey40')
})


# Run Ltt
# 

lttrun <- eventReactive( input$runDiverLtt,{
  
  ltt(tree = treeDiverLTT(),plot = FALSE,drop.extinct = input$dropExtinct[1], log.lineages = input$logLineages, gamma = input$GammaPybus[1])
})   


#Temporal object to print in info panel
# info: tree
observeEvent(input$runDiverLtt, {
  DiverLtt$iterObjectDiver <- lttrun()
})


# Plot Ltt
# 


observeEvent(input$runDiverLtt,{
  output$PhyloPlotDiver3 <- renderPlot({
    
    plot(lttrun(), axes = FALSE, log.lineages = input$logLineages[1], xlab = "time (mybp)")
    axis(2, las = 2, cex.axis = 0.8)
    labs <- axTicks(1)
    h <- max(nodeHeights(treeDiverLTT()))
    at <- h - labs
    axis(1, at = at, labels = labs, cex.axis = 0.8)
    clip(x1 = 0,x2 = h,y1 = 0,y2 = Ntip(treeDiverLTT()))
    grid()
  })
})


### Run MCCR
#
MCCRDiverltt <- eventReactive(input$runMCCRDiverLtt,{
 mccr(lttrun(),rho = as.numeric(input$RhoMCCRdiverLtt[1]),nsim = as.numeric(input$nsimMCCRdiverLtt))
})


# Plot MCCR
# 
observeEvent(input$runMCCRDiverLtt,{
  output$PhyloPlotDiver4 <- renderPlot({
    plot(MCCRDiverltt(),las = 1,cex.axis = 0.8)
  })
})


#Temporal object to print in info panel
# info: MCCR
observeEvent(input$runDiverLtt,{
  DiverLtt$iterObjectDiver <- MCCRDiverltt()
})

