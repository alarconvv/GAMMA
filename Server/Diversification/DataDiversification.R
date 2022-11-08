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
  if (input$treeDiver == 'exampDiverUltra') {
    readRDS(file = 'data/Example.Ultrametric.RDS')
  } else if (input$treeDiver == 'exampDivernonUltra'){
    readRDS(file = 'data/Example.noUltrametric.RDS')
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

ultrametricDiverDT <- eventReactive(input$ultrametricDiverDT1,{
  is.ultrametric(treeInputDiver())
})

#Temporal object to print in info panel
# info: tree
observeEvent(input$ultrametricDiverDT1, {
  
  if (ultrametricDiverDT() == T){
    DiverData$iterObjectDiver <- paste('Is an ultrametric tree? ',ultrametricDiverDT())
    
    treeInputDiver2 <- reactive(treeInputDiver())
    
  } else if (ultrametricDiverDT() == F) {
    
    h <- diag(vcv(treeInputDiver()))
    cvBranch <- round(sd(h)/mean(h),digits = 2)*100
    
    DiverData$iterObjectDiver <- paste('Is an ultrametric tree? ',ultrametricDiverDT(), '-', 'Coeffient of variation among branches:', cvBranch, '%' )

        output$forceultrDiverDT <- renderUI({
      selectInput(inputId = 'frcUltButtDiverDT', label = 'Force ultrametricity',
                  choices = c('Select' = 'selectDiverDT', 'Round decimals' = 'rounddiverDT', 'Chronos dating' = 'chronosDiverDt'))
    })
        output$RunUltrDiverDT <- renderUI({
          actionButton('ButtonUltra', 'Solve Ultrametricity')
        })
        
  }

})


#Chronos table

ageTableDiverDT <- eventReactive(input$importAgeCSV,{
  
  read.csv(file = input$fileageDiverDT$datapath,header = T)
  
})

#Temporal object to print in info panel
# info: tree
observeEvent(input$importAgeCSV, {
  DiverData$iterObjectDiver <- ageTableDiverDT()
})





#Round branches

treeInputDiver2 <- eventReactive(input$ButtonUltra,{
  if  (input$frcUltButtDiverDT == 'rounddiverDT'){

      force.ultrametric(tree = treeInputDiver(),method = 'nnls' )
  
  }else if (input$frcUltButtDiverDT == 'chronosDiverDt'){
    validate(
      need(try(ageTableDiverDT()),"Please, insert ages ")
    )
    
    calibration<-makeChronosCalib(treeInputDiver(),node=ageTableDiverDT()[,1],
                                  age.min=ageTableDiverDT()[,2],age.max=ageTableDiverDT()[,3])
    
    
    
    chronos(treeInputDiver(),calibration=calibration,)
    
  }
  

})


#Temporal object to print in info panel
# info: tree
observeEvent(input$ButtonUltra, {
  DiverData$iterObjectDiver <- treeInputDiver2()
})




#Temporal object to print in info panel
# info: tree
observeEvent(input$ButtonUltra, {
  try(treeInputDiver2())
  h <- diag(vcv(treeInputDiver2()))
  cvBranch <- round(sd(h)/mean(h),digits = 2)*100
  
  isUltra <- is.ultrametric(treeInputDiver2())
  
  DiverData$iterObjectDiver <- paste('Is an ultrametric tree? ',isUltra, '-', 'Coeffient of variation among branches:', cvBranch, '%' )
})






# Resolve polytomies
# 

binaryDiverDT <- eventReactive(input$BinaryDiverDT,{
  is.binary(treeInputDiver2())
})



#Temporal object to print in info panel
# info: tree
observeEvent(input$BinaryDiverDT, {
  
  if (binaryDiverDT() == T){
    DiverData$iterObjectDiver <- paste('Is a binary tree? ', binaryDiverDT())
    
    treeInputDiver3 <- reactive(treeInputDiver2())
    
  } else if (binaryDiverDT() == F) {
    DiverData$iterObjectDiver <- paste('Is a binary tree? ', binaryDiverDT(), '-', 'Please, solve the polytomic nodes' )
    
    output$PoliDiverDT <- renderUI({
      selectInput("ResolveDiverDT", "How to resolve multichotomies", choices = c('Resolve randomly' = 'ramdom','how they appear in the tree' = 'order' ))
    })
    
    output$ButtonPoliDiverDT <- renderUI({
      actionButton('ButtonPoliDiverDT', 'Solve polytomy')
    })
    
  }
  
})



#Solving polytomies

treeInputDiver3 <- eventReactive(input$ButtonPoliDiverDT,{
  if (input$ResolveDiverDT == 'ramdom'){
    multi2di(treeInputDiver2(), random = T)
  } else if (input$ResolveDiverDT == 'order' ){
    multi2di(treeInputDiver2(), random = F)
  }
})




#Temporal object to print in info panel
# info: tree
observeEvent(input$ButtonPoliDiverDT, {
  try(treeInputDiver3())
 
  isBinary <- is.binary(treeInputDiver3())
  
  DiverData$iterObjectDiver <- paste('Is a binary tree? ',isBinary )
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
