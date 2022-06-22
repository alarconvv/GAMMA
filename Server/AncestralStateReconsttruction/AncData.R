################################################################################
#   Data panel
################################################################################




#Temporal objects
#

AncData <- reactiveValues()
AncData$data <- NULL
AncData$clss <- NULL
AncData$numvar <- NULL
AncData$factvar <- NULL


#Render print in Info panel: Data panel
#
output$objects <- renderPrint( {
  if (!is.null(AncData$clss)){
    if (AncData$clss == 'data.frame' || AncData$clss == 'matrix' || AncData$clss == 'table') {
      head(AncData$data, 10L)
    } 
  }else {
    print(AncData$data)
  }
}
)

# set seed for the all session
#
observeEvent(!is.null(input$seed),{
  set.seed(input$seed[1])
})




# Read phylogeny
#
# treeInput <- eventReactive(input$importTree, {
#   
#   if (input$tree == 'examp') {
#     readRDS(file = 'data/anoleTree.RDS')
#   } else if (input$tree == 'treeFile') {
#     if (input$format =='Nexus'){
#       read.nexus(file = input$fileTree$datapath)
#     } else if (input$format == 'Newick'){
#       read.tree(file = input$fileTree$datapath)
#     }
#   } else {  validate(
#     need(try(input$importTree), "Please select a data set")
#   )
#     c('You should choose a tree')
#   }
# }
# )

treeInput <- eventReactive(input$importTree, {
  
  if (input$tree == 'examp') {
    readRDS(file = 'data/anoleTree.RDS')
  } else if (input$tree == 'treeFile' ) {
    
    validate(
      need(try(input$fileTree$datapath), "Please, select a tree ")
    )
    
    con <- file(input$fileTree$datapath,"r")
    first_line <- readLines(con,n=2)
    findNexus <-grep(pattern = '#NEXUS|#Nexus|#nexus',x = first_line )
    close(con)
    if(length(findNexus) == 1){
      read.nexus(file = input$fileTree$datapath)
    }else{
      read.tree(file = input$fileTree$datapath)
    }
    
  }
       
      
}
)






#Temporal object to print in info panel
# info: tree
observeEvent(input$importTree, {
  if(input$tree != 'select'){
    AncData$data <- treeInput()
  }else{
    AncData$data <- c('Please, select a tree')
  }
})


# Reading character file
#
CharInput <- eventReactive(input$importCSV, {
  if (input$csvData == 'exampCSV'){
    readRDS(file = 'data/anoleData.RDS')
  } else if (input$csvData == 'DataFile'){
    
    validate(
      need(try(input$fileCSV$datapath),"Please, select a data set ")
    )
    
    read.csv(file = input$fileCSV$datapath,header = T, row.names = 1)
  } 
})

# tooltip message('Please, be sure that CSV data has headers such as "Species", name.Character1, name.Character2, ... etc')


#Temporal objects to print in info panel
#info: dataframe with characters
#
observeEvent(input$importCSV, {
if(input$csvData != 'select'){
  AncData$data <- CharInput()
  AncData$clss <- class(x = AncData$data)
}else if(is.null(input$fileCSV$datapath) | input$csvData == 'select'){
  AncData$data <- c('Please, select a data set')
}

})


# Check names in tree and character
#
checkNames <- eventReactive(input$checknames, {
  if (length(which(treeInput()$tip.label %in% row.names(x = CharInput()) == F)) == 0) {
    c('Tiplabels OK.')
  } else {
    paste('These tiplables do not have data:',
          c(treeInput$tip.label[(treeInput()$tip.label %in% row.names(CharInput()) == F)]),
          'please, load a valid CSV.Data')
  }
})


#Temporal object to print in info panel
#info: confirm if tree and cvs have the same names
#
observeEvent(input$checknames, {
  AncData$data <- checkNames()
  AncData$clss <- class(AncData$data)
})


#Plot tree if checkbox is clicked
#
heightDt <- reactive(input$PlotHeightDt[1])
widthDt <- reactive(input$PlotWidthDt[1])

output$PhyloPlot <- renderPlot( height = heightDt  , width = widthDt,{
  req(treeInput())


    rawPhylo <- plot.phylo(treeInput(), show.tip.label = input$tipLabels[1],
               cex = input$tipSize[1],use.edge.length = input$branchLength[1], type = input$plotType,
               edge.width = 0.8,edge.color = 'grey40')
    return(rawPhylo)
  
})


#Dynamic input selection (Variables)
#Note: when there is only one option you should use list(), but there are more than one you could code names() straightforward
#
observeEvent(CharInput(), {
  AncData$numvar <- which(lapply(CharInput(), class) == 'numeric' | lapply(CharInput(), class) == 'integer')
  AncData$factvar <- which(lapply(CharInput(), class) == 'factor' | lapply(CharInput(), class) == 'character')
  if (length(AncData$numvar) == 0) {
    if (length(AncData$factvar) > 1) {
      updateSelectInput(session, "dataVar",
                        choices=list('Select','Discrete Char'= names(AncData$factvar)))
    } else {
      updateSelectInput(session, "dataVar",
                        choices=list('Select','Discrete Char'= list(names(AncData$factvar))))
    }
  } else if (length(AncData$factvar) == 0) {
    if (length(AncData$numvar) > 1){
      updateSelectInput(session, "dataVar",
                        choices=list('Select','Continuou Char'= names(AncData$numvar)))
    }else{
      updateSelectInput(session, "dataVar",
                        choices=list('Select','Continuou Char'= list(names(AncData$numvar))))
    }
  } else {
    if (length(AncData$factvar) > 1 & length(AncData$numvar) == 1) {
      updateSelectInput(session, "dataVar",
                        choices = list('Select','Discrete Char'= names(AncData$factvar),
                                       'Continuou Char'= list(names(AncData$numvar))))
    } else if (length(AncData$factvar) == 1 & length(AncData$numvar) > 1) {
      updateSelectInput(session, "dataVar",
                        choices=list('Select','Discrete Char'= list(names(AncData$factvar)),
                                     'Continuou Char'= names(AncData$numvar)))
    } else if (length(AncData$factvar) == 1 & length(AncData$numvar) == 1) {
      updateSelectInput(session, "dataVar",
                        choices=list('Select','Discrete Char'= list(names(AncData$factvar)),'
                                               Continuou Char'= list(names(AncData$numvar))))
    } else {
      updateSelectInput(session, "dataVar",
                        choices=list('Select','Discrete Char'= names(AncData$factvar),
                                     'Continuou Char'= names(AncData$numvar)))
    }
  }
})


#Classify character in their different types
#
ClassCol <- eventReactive(input$dataVar, {
  if (class(CharInput()[,which(colnames(CharInput()) == input$dataVar)]) == 'numeric') {
    c('CONTINUOUS character. Please, confirm how to analyze it')
  } else if (class(CharInput()[,which(colnames(CharInput()) == input$dataVar)]) == 'factor') {
    c('DISCRETE character. Please, confirm how to analyze it')
  } else {
    c('You should select a Character')
  }
})


#Temporal object to print in info panel
#info: print the type of character selected
#
observe( {
  if (!is.null(ClassCol())) {
    AncData$data <- ClassCol()
  }
})


#Confirm the type of selected column
ConfirmClassCol <- eventReactive(input$typeChar, {
  if (input$typeChar == 'Continuous') {
    c('CONTINUOUS character. Data successfully loaded. Please, go to Analysis tab')
  } else if (input$typeChar == 'Discrete') {
    c('DISCRETE character. Data successfully loaded. Please, go to Analysis tab')
  } else if(input$typeChar == 'Select'){
    c('You should select a Character')
  }
})


#Temporal object to print in info panes
#info: print how the character will be analyzed
observe( {
  if (!is.null(ConfirmClassCol())) {
    AncData$data <- ConfirmClassCol()
  }
})


#Assign the character to a variable and confirm the object class
# Continuous character
#


SelectedVar <- eventReactive(c(input$typeChar == 'Continuous', input$dataVar != 'Select'), {
  shiny::req(input$dataVar != 'Select' | !is.null(input$dataVar))
    as.numeric(CharInput()[, which(colnames(CharInput()) == input$dataVar)])
  })


#Discrete character
#

  SelectedVarDisc <- eventReactive(c(input$typeChar == 'Discrete', input$dataVar != 'Select'), {
    shiny::req(input$dataVar != 'Select' | !is.null(input$dataVar))
      as.factor(CharInput()[, which(colnames(CharInput()) == input$dataVar)])
  })


