

#Temporal objects
v <- reactiveValues()
v$data <- NULL
v$clss <- NULL
v$numvar <- NULL
v$factvar <- NULL
v$objetContinuousML <- NULL
v$Models <- list()
v$objectContinuousBI <- NULL
v$counterModelDisML <- NULL
v$objectDiscreteML <- NULL
v$objectDiscreteBI <- NULL


#Render print in Info panel: Data panel
#
output$objects <- renderPrint( {
  if (!is.null(v$clss)){
    if (v$clss == 'data.frame' || v$clss == 'matrix' || v$clss == 'table') {
      head(v$data, 10L)
    } else {
      print(v$data)
    }
  }
}
)


# Read phylogeny
#
treeInput <- eventReactive(input$importTree, {
  if (input$tree == 'examp') {
    readRDS(file = 'data/anoleTree.RDS')
  } else if (input$tree == 'treeFile') {
    if (input$format =='Nexus'){
      read.nexus(file = input$fileTree$datapath)
    } else if (input$format == 'Newick'){
      read.tree(file = input$fileTree$datapath)
    }
  } else {
    c('You should choose a tree')
  }
}
)


#Temporal object to print in info panel
# info: tree
observeEvent(input$importTree, {
  v$data <- treeInput()
  v$clss <- class(x = v$data)
})


# Reading character file
#
CharInput <- eventReactive(input$importCSV, {
  if (input$csvData == 'exampCSV'){
    readRDS(file = 'data/anoleData.RDS')
  } else if (input$csvData == 'DataFile'){
    read.csv(file = input$fileCSV$datapath,header = T, row.names = 1)
  } else {
    c('You should select data' )
  }
})

# tooltip message('Please, be sure that CSV data has headers such as "Species", name.Character1, name.Character2, ... etc')


#Temporal objects to print in info panel
#info: dataframe with characters
#
observeEvent(input$importCSV, {
  v$data <- CharInput()
  v$clss <- class(x = v$data)
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
  v$data <- checkNames()
  v$clss <- class(v$data)
})


#Plot tree if checkbox is clicked
#
output$PhyloPlot <- renderPlot( {
  if (input$plottree == T) {
    plot.phylo(treeInput(), show.tip.label = input$tipLabels[1],
               cex = input$tipSize[1],use.edge.length = input$branchLength[1])
  }
})


#Dynamic input selection (Variables)
#Note: when there is only one option you should use list(), but there are more than one you could code names() straightforward
#
observeEvent(CharInput(), {
  v$numvar <- which(lapply(CharInput(), class) == 'numeric' | lapply(CharInput(), class) == 'integer')
  v$factvar <- which(lapply(CharInput(), class) == 'factor' | lapply(CharInput(), class) == 'character')
  if (length(v$numvar) == 0) {
    if (length(v$factvar) > 1) {
      updateSelectInput(session, "dataVar",
                        choices=list('Select','Discrete Char'= names(v$factvar)))
    } else {
      updateSelectInput(session, "dataVar",
                        choices=list('Select','Discrete Char'= list(names(v$factvar))))
    }
  } else if (length(v$factvar) == 0) {
    if (length(v$numvar) > 1){
      updateSelectInput(session, "dataVar",
                        choices=list('Select','Continuou Char'= names(v$numvar)))
    }else{
      updateSelectInput(session, "dataVar",
                        choices=list('Select','Continuou Char'= list(names(v$numvar))))
    }
  } else {
    if (length(v$factvar) > 1 & length(v$numvar) == 1) {
      updateSelectInput(session, "dataVar",
                        choices = list('Select','Discrete Char'= names(v$factvar),
                                       'Continuou Char'= list(names(v$numvar))))
    } else if (length(v$factvar) == 1 & length(v$numvar) > 1) {
      updateSelectInput(session, "dataVar",
                        choices=list('Select','Discrete Char'= list(names(v$factvar)),
                                     'Continuou Char'= names(v$numvar)))
    } else if (length(v$factvar) == 1 & length(v$numvar) == 1) {
      updateSelectInput(session, "dataVar",
                        choices=list('Select','Discrete Char'= list(names(v$factvar)),'
                                               Continuou Char'= list(names(v$numvar))))
    } else {
      updateSelectInput(session, "dataVar",
                        choices=list('Select','Discrete Char'= names(v$factvar),
                                     'Continuou Char'= names(v$numvar)))
    }
  }
})


#Classify character in their different types
#
ClassCol <- eventReactive(input$dataVar, {
  if (class(CharInput()[,which(colnames(CharInput()) == input$dataVar)]) == 'numeric') {
    c('Numerical class. Please, confirm how to analyze it: as a Discrete or a Contiuous character')
  } else if (class(CharInput()[,which(colnames(CharInput())== input$dataVar)])== 'factor') {
    c('Factor class. Please, confirm how to analyze it: as a Discrete or a Contiuous character')
  } else {
    c('You should select a Character')
  }
})


#Temporal object to print in info panel
#info: print the type of character selected
#
observe( {
  if (!is.null(ClassCol())) {
    v$data <- ClassCol()
  }
})


#Confirming the type of selected column
ConfirmClassCol <- eventReactive(input$typeChar, {
  if (input$typeChar == 'Continuous') {
    c('CONTINUOUS character. Data successfully loaded. Please, go to Analysis tab')
  } else if (input$typeChar == 'Discrete') {
    c('DISCRETE character. Data successfully loaded. Please, go to Analysis tab')
  } else {
    c('You should select a Character')
  }
})


#Temporal object to print in info panes
#info: print how the character will be analized
observe( {
  if (!is.null(ConfirmClassCol())) {
    v$data <- ConfirmClassCol()
  }
})


#Assign the character to a variable and confirm the object class
# Continuous character
#
SelectedVar <- eventReactive(input$typeChar == 'Continuous', {
  as.numeric(CharInput()[, which(colnames(CharInput()) == input$dataVar)])
})

#Discrete character
#
SelectedVarDisc <- eventReactive(input$typeChar == 'Discrete', {
  as.factor(CharInput()[, which(colnames(CharInput()) == input$dataVar)])
})

