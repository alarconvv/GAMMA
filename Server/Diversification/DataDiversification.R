################################################################################
#   Data panel Diversification
################################################################################


#Temporal objects
#
DiverData <- reactiveValues()
DiverData$iterObjectDiver <- NULL



#Render print in Info panel: Data panel
#
output$infoPanelDiverData <- renderPrint( {
  print(DiverData$iterObjectDiver)
  })

# set seed for the all session
#
observeEvent(!is.null(input$seedDiverDT),{
  set.seed(input$seedDiverDT[1])
})





#Load tree or an example
#
treeInputDiver <- eventReactive( input$importTreeDiver, {
  if (input$treeDiver == 'exampDiver') {
    readRDS(file = 'data/ExampleDiver')
    } else if (input$treeDiver == 'treeFileDiver' ) {
      validate(need(try(input$fileTreeDiver$datapath), "Please, select a tree "))
      pathDiver <- file(input$fileTreeDiver$datapath,"r")
      first_lineDiver <- readLines(pathDiver,n = 2)
      findNexusDiver <- grep(pattern = '#NEXUS|#Nexus|#nexus',x = first_lineDiver )
      close(pathDiver)
      if (length(findNexusDiver) == 1) {
        read.nexus(file = input$fileTreeDiver$datapath)
        } else {
          read.tree(file = input$fileTreeDiver$datapath)
        }
    }
  })




#Temporal object to print in info panel
# info: tree
observeEvent(input$importTree, {
  DiverData$iterObjectDiver <- treeInputDiver()
})






# Is ultrametric

ultrametricDiverDT <- eventReactive(input$ultrametricDiverDT,{
  is.ultrametric(treeInputDiver())
})


observeEvent(ultrametricDiverDT() == F,{
    output$forceultrDiverDT <- renderUI({
      selectInput(inputId = 'frcUltButtDiverDT', label = 'Force ultrametricity',
                    choices = c('Select' = 'selectDiverDT', 'Round decimals' = 'rounddiverDT', 'Chronos dating' = 'chronosDiverDt') )
    })
})


#Temporal object to print in info panel
# info: tree
observeEvent(input$ultrametricDiverDT, {
  DiverData$iterObjectDiver <- paste('Is your tree ultrametric? ',ultrametricDiverDT())
})




# Resolve politomies
# 

binaryTree <- eventReactive(input$BinaryDiverDT,{
  is.binary(treeInputDiver())
})

observeEvent(binaryTree() == F, {
  output$PoliDiverDT <- renderUI({
    checkboxInput("PoliDiverDT", "Resolve politomic nodes")
  })
})





#Plot tree
#
heightDiverDT <- reactive(input$PlotHeightDiverDT[1])
widthDiverDT <- reactive(input$PlotWidthDiverDT[1])

output$PhyloPlotDiver1 <- renderPlot( height = heightDiverDT  , width = widthDiverDT,{
  req(treeInputDiver())
  
  
  rawPhylo <- plot.phylo(treeInputDiver(), show.tip.label = input$tipLabelsDiverDT[1],
                         cex = input$tipSizeDiverDT[1], use.edge.length = input$branchLengthDiverDT, 
                          type = input$plotTypeDiverDT,
                         edge.width = 0.8,edge.color = 'grey40')
  return(rawPhylo)
  
})
