#Render print in Info panel: ML Analysis
#
output$infoPanelDiscreteBI <- renderPrint({
  if (!is.null(v$objectDiscreteBI)) {
    print(v$objectDiscreteBI)
  }
})


# Vector to store matrices, vectors and output from Anc Discrete BI analysis
# 
AncDiscreteBI <- reactiveValues()
AncDiscreteBI$modelMatrixBI <- NULL


# Count the number of states of the character to analyse
# 
nStatesBI <- reactive(length(levels(SelectedVarDisc())))





# Update selectInput to the models which are depending on the number of states
# 
observeEvent(input$typeChar == 'Discrete', {
if (nStates()[1] == 2) {
  # update model list
  updateSelectInput(session, "ModelsDisBI", 
                    choices = c('ER' = 'ER', 'ARD' = 'ARD', 
                                'Ireversible01' = 'Ireversible01', 
                                'Ireversible10' = 'Ireversible10'))
  } else {
    #update model list for 3 states
    updateSelectInput(session, "ModelsDisBI",
                    choices = c('ER' = 'ER', 'ARD' = 'ARD', 'SYM' = 'SYM', 'Costumize' = 'Costumize'))
  }

  }) 



# Create matrices depending on the state number for predeterminer models
# 
observeEvent(!is.null(input$ModelsDisBI),{ 
  
  AncDiscreteBI$matrix0 <- matrix(data = as.integer(0), nrow = nStatesBI()[1], ncol = nStatesBI()[1], 
                                           dimnames = list(levels(SelectedVarDisc()),levels(SelectedVarDisc())))

  # Create ER model
  # 
  if (input$ModelsDisBI == 'ER') {
    AncDiscreteBI$modelMatrixBI <- AncDiscreteBI$matrix0 # assign a 0 matrices to create a model
    row.names(AncDiscreteBI$modelMatrixBI) <- levels(SelectedVarDisc()) # assign name states
    colnames(AncDiscreteBI$modelMatrixBI) <- levels(SelectedVarDisc())
    AncDiscreteBI$modelMatrixBI[lower.tri(AncDiscreteBI$modelMatrixBI)] <- 1 # only 1 type of rate
    AncDiscreteBI$modelMatrixBI[upper.tri(AncDiscreteBI$modelMatrixBI)] <- 1
    names(AncDiscreteBI$modelMatrixBI) <- 'ER'
  }
  # Create ARD model
  # 
  if (input$ModelsDisBI == 'ARD') {
    AncDiscreteBI$modelMatrixBI <- AncDiscreteBI$matrix0 # assign a 0 matrices to create a model
    row.names(AncDiscreteBI$modelMatrixBI) <- levels(SelectedVarDisc()) # assign name states
    colnames(AncDiscreteBI$modelMatrixBI) <- levels(SelectedVarDisc())
    AncDiscreteBI$modelMatrixBI[lower.tri(AncDiscreteBI$modelMatrixBI)] <- 2 # 2 type of rates
    AncDiscreteBI$modelMatrixBI[upper.tri(AncDiscreteBI$modelMatrixBI)] <- 1
    names(AncDiscreteBI$modelMatrixBI) <- 'ARD'
    }
  # Create  Ireversible01
  # 
  if (input$ModelsDisBI == 'Ireversible01') {
    AncDiscreteBI$modelMatrixBI <- AncDiscreteBI$matrix0 # assign a 0 matrices to create a model
    row.names(AncDiscreteBI$modelMatrixBI) <- levels(SelectedVarDisc()) # assign name states
    colnames(AncDiscreteBI$modelMatrixBI) <- levels(SelectedVarDisc())
    AncDiscreteBI$modelMatrixBI[lower.tri(AncDiscreteBI$modelMatrixBI)] <- 1 # only 1 type of rate
    AncDiscreteBI$modelMatrixBI[upper.tri(AncDiscreteBI$modelMatrixBI)] <- 0
    names(AncDiscreteBI$modelMatrixBI) <- 'Ireversible01'
    }
  # Create Ireversible10
  # 
  if (input$ModelsDisBI == 'Ireversible10') {
    AncDiscreteBI$modelMatrixBI <- AncDiscreteBI$matrix0 # assign a 0 matrices to create a model
    row.names(AncDiscreteBI$modelMatrixBI) <- levels(SelectedVarDisc()) # assign name states
    colnames(AncDiscreteBI$modelMatrixBI) <- levels(SelectedVarDisc())
    AncDiscreteBI$modelMatrixBI[lower.tri(AncDiscreteBI$modelMatrixBI)] <- 0 # only 1 type of rate
    AncDiscreteBI$modelMatrixBI[upper.tri(AncDiscreteBI$modelMatrixBI)] <- 1
    names(AncDiscreteBI$modelMatrixBI) <- 'Ireversible10'
    }
  # Create SYM model
  # 
  if (input$ModelsDisBI == 'SYM') {
    AncDiscreteBI$modelMatrixBI <- AncDiscreteBI$matrix0 # assign a 0 matrices to create a model
    row.names(AncDiscreteBI$modelMatrixBI) <- levels(SelectedVarDisc()) # assign name states
    colnames(AncDiscreteBI$modelMatrixBI) <- levels(SelectedVarDisc())
    #Different rate per each pair 
    AncDiscreteBI$modelMatrixBI[lower.tri(AncDiscreteBI$modelMatrixBI)] <- 1:length(AncDiscreteBI$modelMatrixBI[lower.tri(AncDiscreteBI$modelMatrixBI)])
    AncDiscreteBI$modelMatrixBI[upper.tri(AncDiscreteBI$modelMatrixBI)] <- 1:length(AncDiscreteBI$modelMatrixBI[lower.tri(AncDiscreteBI$modelMatrixBI)])
    names(AncDiscreteBI$modelMatrixBI) <- 'SYM'
    }
  # Costumize rate types and create a new model
  # 
  if (input$ModelsDisBI == 'Costumize') {
    AncDiscreteBI$modelMatrixBI <- NULL
    output$costuModelBI <- renderRHandsontable( {# add matrix for a new model
      AncDiscreteBI$matrix0[lower.tri(AncDiscreteBI$matrix0)] <- as.integer(1)
      AncDiscreteBI$matrix0[upper.tri(AncDiscreteBI$matrix0)] <- as.integer(1)
      rhandsontable(AncDiscreteBI$matrix0,readOnly = F)
      
    })
    }
    
    
})


  observeEvent(input$SubmAddModelBI > 0, {
    # # Submit new model
    AncDiscreteBI$modelMatrixBI <- hot_to_r(input$costuModelBI) # get values from costumizable table
    row.names(AncDiscreteBI$modelMatrixBI) <- levels(SelectedVarDisc()) # assign name states
    colnames(AncDiscreteBI$modelMatrixBI) <- levels(SelectedVarDisc())
    # # Name new model
     names(AncDiscreteBI$modelMatrixBI) <- 'UserName'
     
     v$objectDiscreteBI <- AncDiscreteBI$modelMatrixBI
  })



# Temporal object to print in info panel
# info: model
#
observeEvent(!is.null(input$ModelsDisBI),{
  v$objectDiscreteBI <- AncDiscreteBI$modelMatrixBI
})


#Set Q matriz parameter
#
observeEvent(input$QmatrixBI != 'select', {
  if (input$QmatrixBI == 'costumQmDiscBI') {
    AncDiscreteBI$matrix0 <- matrix(data = as.numeric(0), nrow = nStatesBI()[1], ncol = nStatesBI()[1],
                                             dimnames = list(levels(SelectedVarDisc()),levels(SelectedVarDisc())))
    output$QmatBI <- renderRHandsontable( {# add matrix for a new Qmatrix
      AncDiscreteBI$matrix0[lower.tri(AncDiscreteBI$matrix0)] <- as.numeric(0.5)
      AncDiscreteBI$matrix0[upper.tri(AncDiscreteBI$matrix0)] <- as.numeric(0.5)
      diag(AncDiscreteBI$matrix0) <- -rowSums(AncDiscreteBI$matrix0,na.rm = TRUE)
      rhandsontable(AncDiscreteBI$matrix0,readOnly = F)})
  } else {
    AncDiscreteBI$QmatrixPar <- input$QmatrixBI
  }
})

# Submit Q matrix
#
observeEvent(input$SubmQmatBI > 0,{
  AncDiscreteBI$QmatrixPar <-  hot_to_r(input$QmatBI) # get values from costumizable table
})

