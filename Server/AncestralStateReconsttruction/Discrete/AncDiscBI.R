##############################################################################
# Discrete Character : Stochastic Mapping
##############################################################################


#Render print in Info panel: ML Analysis
#
output$infoPanelDiscreteBI <- renderPrint({
  if (!is.null(AncDiscreteBI$objectDiscreteBI)) {
    print(AncDiscreteBI$objectDiscreteBI)
  }
})


# Vector to store matrices, vectors and output from Anc Discrete BI analysis
# 
AncDiscreteBI <- reactiveValues()

AncDiscreteBI$objectDiscreteBI <- NULL
AncDiscreteBI$modelMatrixBI <- NULL
AncDiscreteBI$QmatrixPar <- NULL
AncDiscreteBI$PiprobBI <- NULL
AncDiscreteBI$nsim <- NULL
#AncDiscreteBI$outputDisBI <- NULL
AncDiscreteBI$sampleFreq <- NULL
AncDiscreteBI$burnin <- NULL
AncDiscreteBI$Priorpar <- NULL
AncDiscreteBI$vQ <- NULL
AncDiscreteBI$Density <- NULL
AncDiscreteBI$setCharacter <- NULL



#Tree

treeDisBI  <- eventReactive(c( treeInput()),{
  validate(
    need(try(treeInput()), "Please select a tree")
  )
  treeInput()
}
)

#Tree

DataDisBI  <- eventReactive(c(SelectedVarDisc()),{
  validate(
    need(try(SelectedVarDisc()), "Please select a data set")
  )
  SelectedVarDisc()
}
)



# Count the number of states of the character to analyse
# 


nStatesBI <- eventReactive(c(DataDisBI()),{length(levels(DataDisBI()))})


# set character
observeEvent(DataDisBI(),{
  AncDiscreteBI$setCharacter <- setNames(DataDisBI(),row.names(CharInput()))
})



# Update selectInput to the models which are depending on the number of states
# 
observeEvent(c(DataDisBI()), {
 
  
  
if (nStatesBI()[1] == 2) {
  # update model list
  updateSelectInput(session, "ModelsDisBI", 
                    choices = c('Select' = 'Select','ER' = 'ER', 'ARD' = 'ARD', 
                                'Ireversible01' = 'Ireversible01', 
                                'Ireversible10' = 'Ireversible10'))
  } else {
    #update model list for 3 states
    updateSelectInput(session, "ModelsDisBI",
                    choices = c('Select' = 'Select','ER' = 'ER', 'ARD' = 'ARD', 'SYM' = 'SYM', 'Costumize' = 'Costumize'))
  }

  }) 



# Create matrices depending on the state number for predeterminer models
# 
observeEvent(!is.null(input$ModelsDisBI),{ 
  
  AncDiscreteBI$matrix0 <- matrix(data = as.integer(0), nrow = nStatesBI()[1], ncol = nStatesBI()[1], 
                                           dimnames = list(levels(DataDisBI()),levels(DataDisBI())))

  # Create ER model
  # 
  if (input$ModelsDisBI == 'ER') {
    AncDiscreteBI$modelMatrixBI <- AncDiscreteBI$matrix0 # assign a 0 matrices to create a model
    row.names(AncDiscreteBI$modelMatrixBI) <- levels(DataDisBI()) # assign name states
    colnames(AncDiscreteBI$modelMatrixBI) <- levels(DataDisBI())
    AncDiscreteBI$modelMatrixBI[lower.tri(AncDiscreteBI$modelMatrixBI)] <- 1 # only 1 type of rate
    AncDiscreteBI$modelMatrixBI[upper.tri(AncDiscreteBI$modelMatrixBI)] <- 1
    names(AncDiscreteBI$modelMatrixBI) <- 'ER'
  }
  # Create ARD model
  # 
  if (input$ModelsDisBI == 'ARD') {
    AncDiscreteBI$modelMatrixBI <- AncDiscreteBI$matrix0 # assign a 0 matrices to create a model
    row.names(AncDiscreteBI$modelMatrixBI) <- levels(DataDisBI()) # assign name states
    colnames(AncDiscreteBI$modelMatrixBI) <- levels(DataDisBI())
    AncDiscreteBI$modelMatrixBI[lower.tri(AncDiscreteBI$modelMatrixBI)] <- 2 # 2 type of rates
    AncDiscreteBI$modelMatrixBI[upper.tri(AncDiscreteBI$modelMatrixBI)] <- 1
    names(AncDiscreteBI$modelMatrixBI) <- 'ARD'
    }
  # Create  Ireversible01
  # 
  if (input$ModelsDisBI == 'Ireversible01') {
    AncDiscreteBI$modelMatrixBI <- AncDiscreteBI$matrix0 # assign a 0 matrices to create a model
    row.names(AncDiscreteBI$modelMatrixBI) <- levels(DataDisBI()) # assign name states
    colnames(AncDiscreteBI$modelMatrixBI) <- levels(DataDisBI())
    AncDiscreteBI$modelMatrixBI[lower.tri(AncDiscreteBI$modelMatrixBI)] <- 1 # only 1 type of rate
    AncDiscreteBI$modelMatrixBI[upper.tri(AncDiscreteBI$modelMatrixBI)] <- 0
    names(AncDiscreteBI$modelMatrixBI) <- 'Ireversible01'
    }
  # Create Ireversible10
  # 
  if (input$ModelsDisBI == 'Ireversible10') {
    AncDiscreteBI$modelMatrixBI <- AncDiscreteBI$matrix0 # assign a 0 matrices to create a model
    row.names(AncDiscreteBI$modelMatrixBI) <- levels(DataDisBI()) # assign name states
    colnames(AncDiscreteBI$modelMatrixBI) <- levels(DataDisBI())
    AncDiscreteBI$modelMatrixBI[lower.tri(AncDiscreteBI$modelMatrixBI)] <- 0 # only 1 type of rate
    AncDiscreteBI$modelMatrixBI[upper.tri(AncDiscreteBI$modelMatrixBI)] <- 1
    names(AncDiscreteBI$modelMatrixBI) <- 'Ireversible10'
    }
  # Create SYM model
  # 
  if (input$ModelsDisBI == 'SYM') {
    AncDiscreteBI$modelMatrixBI <- AncDiscreteBI$matrix0 # assign a 0 matrices to create a model
    row.names(AncDiscreteBI$modelMatrixBI) <- levels(DataDisBI()) # assign name states
    colnames(AncDiscreteBI$modelMatrixBI) <- levels(DataDisBI())
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

# submit matrix with a new model
# 
  observeEvent(input$SubmAddModelBI > 0, {
    # # Submit new model
    AncDiscreteBI$modelMatrixBI <- hot_to_r(input$costuModelBI) # get values from costumi table
    row.names(AncDiscreteBI$modelMatrixBI) <- levels(DataDisBI()) # assign name states
    colnames(AncDiscreteBI$modelMatrixBI) <- levels(DataDisBI())
    # # Name new model
     names(AncDiscreteBI$modelMatrixBI) <- 'UserModel'
     
     AncDiscreteBI$objectDiscreteBI <- AncDiscreteBI$modelMatrixBI
  })



# Temporal object to print in info panel
# info: model
#
observeEvent(!is.null(AncDiscreteBI$modelMatrixBI),{
  AncDiscreteBI$objectDiscreteBI <- AncDiscreteBI$modelMatrixBI
})


#Set Q matriz parameter
#
observeEvent(input$QmatrixBI != 'select', {
  AncDiscreteBI$matrix1 <- matrix(data = NA, nrow = nStatesBI()[1], ncol = nStatesBI()[1],
                                  dimnames = list(levels(DataDisBI()),levels(DataDisBI())))
  AncDiscreteBI$matrix1[lower.tri(AncDiscreteBI$matrix1)] <- as.numeric(0.5)
  AncDiscreteBI$matrix1[upper.tri(AncDiscreteBI$matrix1)] <- as.numeric(0.5)

  if (input$QmatrixBI == 'costumQmDiscBI') {
    AncDiscreteBI$QmatrixPar <- NULL
    output$putitamatrix <- renderRHandsontable( {# add matrix for a new Qmatrix
      rhandsontable(AncDiscreteBI$matrix1,readOnly = F)
      })
    } else {
    AncDiscreteBI$QmatrixPar <- input$QmatrixBI
  }
})

# Submit Q matrix
#
observeEvent(input$SubmQmatBI > 0,{
AncDiscreteBI$QmatrixPar <-  hot_to_r(input$putitamatrix)
row.names(AncDiscreteBI$QmatrixPar) <- levels(DataDisBI()) # assign name states
colnames(AncDiscreteBI$QmatrixPar) <- levels(DataDisBI())
diag(AncDiscreteBI$QmatrixPar) <- -rowSums(AncDiscreteBI$QmatrixPar,na.rm = TRUE)# get values from costumizable table
})

# Temporal object to print in info panel
# info: Q matrix
#
observeEvent(!is.null(AncDiscreteBI$QmatrixPar),{
  AncDiscreteBI$objectDiscreteBI <- AncDiscreteBI$QmatrixPar
})


#Set Pi: prior probabilities in the root
#
observeEvent(input$piBI != 'select', {
  AncDiscreteBI$matrix2 <- matrix(data = NA, nrow = 1, ncol = nStatesBI()[1],
                                  dimnames = list('Pi prob.',levels(DataDisBI())))
  AncDiscreteBI$matrix2[1,] <- as.numeric(1/nStatesBI()[1])
 if (input$piBI == 'costumPiDiscBI') {
    AncDiscreteBI$PiprobBI <- NULL
    output$matPiBI <- renderRHandsontable( {# add matrix for a new Qmatrix
      rhandsontable(AncDiscreteBI$matrix2,readOnly = F)
    })
  } else {
    AncDiscreteBI$PiprobBI <- input$piBI
  }
})

# Submit Pi probabilities in the root
#
observeEvent(input$SubmPiBI > 0,{
  tempPi <- hot_to_r(input$matPiBI)
  sumPiProb <- sum(tempPi[1,])
  
  if (sumPiProb == 1) {
    AncDiscreteBI$PiprobBI <-  c(hot_to_r(input$matPiBI))
  }else{
    AncDiscreteBI$objectDiscreteBI <- 'Probabilities must sum 1'
  }

})

# Temporal object to print in info panel
# info: Pi probabilities in the root
#
observeEvent(!is.null(AncDiscreteBI$PiprobBI),{
  AncDiscreteBI$objectDiscreteBI <- AncDiscreteBI$PiprobBI
})


# set number of simulation
# 
observeEvent(!is.null(input$nsimDisBI),{
  AncDiscreteBI$nsim <- as.numeric(input$nsimDisBI)
})

# if mcmc, set sample frequency
# 
observeEvent(!is.null(input$samfreqDisBI),{
  AncDiscreteBI$sampleFreq <- as.numeric(input$samfreqDisBI)
})

# if mcmc, set burning
# 
observeEvent(!is.null(input$samfreqDisBI),{
  AncDiscreteBI$burnin <- as.numeric(input$burninDisBI) * (AncDiscreteBI$nsim * AncDiscreteBI$sampleFreq)
})




# Set Prior parameter if mcmc = costumize, 'useEmpirical' and 'oneBeta'
# 
observeEvent(input$priorDisBI == 'useEmpirical' & input$betaValueDisBI == 'oneBeta',{
  
  AncDiscreteBI$Priorpar <- NULL
  AncDiscreteBI$matrix3.1 <- matrix(data = NA, nrow = 1, ncol = 1,dimnames = list('beta', 'value' ))
  AncDiscreteBI$matrix3.1[1,] <- as.numeric(1)
  
  output$matBetaValDisBI <- renderRHandsontable( {# add matrix for a new Qmatrix
    rhandsontable(AncDiscreteBI$matrix3.1,readOnly = F)
  })

})


# Submit Beta value 
#
observeEvent(input$SubmBetaValDisBI > 0,{
  
  AncDiscreteBI$Priorpar <- list(use.empirical = TRUE,beta = c(hot_to_r(input$matBetaValDisBI)[1,]))
  
})


# Set Prior parameter if mcmc = costumize, 'useEmpirical' and some 'betaPerRate'
#
observeEvent(input$priorDisBI == 'useEmpirical' & input$betavaluesDisBI == 'betaPerRate', {
  AncDiscreteBI$Priorpar <- NULL
  AncDiscreteBI$matrix3 <- matrix(data = NA, nrow = 1, ncol = length(which(unique(c(AncDiscreteBI$modelMatrixBI)) != 0)),
                                  dimnames = list('betas',  sort(unique(c(AncDiscreteBI$modelMatrixBI))[which(unique(c(AncDiscreteBI$modelMatrixBI)) != 0)])))
  AncDiscreteBI$matrix3[1,] <- as.numeric(1)
  
  output$matBetaValuePerRateDisBI <- renderRHandsontable( {# add matrix for a new Qmatrix
    rhandsontable(AncDiscreteBI$matrix3,readOnly = F)
  })

  })


# Submit Beta values per rates
#
observeEvent(input$SubmBetaValuePerRateDisBI > 0,{
  AncDiscreteBI$Priorpar <- list(use.empirical = TRUE,beta = c(hot_to_r(input$matBetaValuePerRateDisBI)[1,]))

})


# Set Prior parameter if mcmc = costumize, 'noUseEmpirical' and 'oneAlphaBeta'
#
observeEvent(input$priorDisBI == 'noUseEmpirical' & input$alphaBetaValDisBI == 'oneAlphaBeta', {
  AncDiscreteBI$matrix4 <- matrix(data = NA, nrow = 2, ncol = 1,
                                  dimnames = list(c('alpha','beta'), 'Value'))
  AncDiscreteBI$matrix4[1,1] <- as.numeric(0.5)
  AncDiscreteBI$matrix4[2,1] <- as.numeric(1)
  
  AncDiscreteBI$Priorpar <- NULL
  
  output$matOneAlphaBetaValDisBI <- renderRHandsontable( {# add matrix for a new Qmatrix
    rhandsontable(AncDiscreteBI$matrix4,readOnly = F)
  })
})

# Submit one alpha and beta values 
#
observeEvent(input$SubmOneAlphaBetaValDisBI > 0,{
  AncDiscreteBI$Priorpar <- list(use.empirical = FALSE,
                                 alpha = hot_to_r(input$matOneAlphaBetaValDisBI)[1,1],
                                 beta = hot_to_r(input$matOneAlphaBetaValDisBI)[2,1])
  
})


# Set Prior parameter if mcmc = costumize, 'noUseEmpirical' and 'oneAlphaBeta'
#

observeEvent(input$priorDisBI == 'noUseEmpirical' & input$alphaBetaValDisBI == 'alphaBetaPerRate', {
  AncDiscreteBI$matrix5 <- matrix(data = NA, nrow = 2, ncol = length(which(unique(c(AncDiscreteBI$modelMatrixBI)) != 0)),
                                  dimnames = list(c('alpha','beta'),sort(unique(c(AncDiscreteBI$modelMatrixBI))[which(unique(c(AncDiscreteBI$modelMatrixBI)) != 0)])))
  AncDiscreteBI$matrix5[1,] <- as.numeric(0.5)
  AncDiscreteBI$matrix5[2,] <- as.numeric(1)
  
  AncDiscreteBI$Priorpar <- NULL
  output$matAlphaBetaPerRateDisBI <- renderRHandsontable( {# add matrix for a new Qmatrix
    rhandsontable(AncDiscreteBI$matrix5,readOnly = F)
  })
})


# Submit one alpha and beta value per rates
#
observeEvent(input$SubmAlphaBetaPerRateDisBI > 0,{
  AncDiscreteBI$Priorpar <- list(use.empirical = FALSE,
                                 alpha = c(hot_to_r(input$matAlphaBetaPerRateDisBI)[1,]),
                                 beta = c(hot_to_r(input$matAlphaBetaPerRateDisBI)[2,]))
  
})


# Temporal object to print in info panel
# info: PRIOR PARAMETERS
#
observeEvent(!is.null(AncDiscreteBI$Priorpar),{
  AncDiscreteBI$objectDiscreteBI <- AncDiscreteBI$Priorpar
})



# Set variance (steps) in for Q matrix: equal value for every rate
# 
observeEvent(input$vQDisBI == 'equalvQDisBI',{
  AncDiscreteBI$matrix6 <- matrix(data = NA, nrow = 1, ncol = 1,dimnames = list('variance', 'value' ))
  AncDiscreteBI$matrix6[1,1] <- as.numeric(0.1)
  AncDiscreteBI$vQ <- NULL
  output$matequalvQValDisBI <- renderRHandsontable( {# add matrix for a new Qmatrix
    rhandsontable(AncDiscreteBI$matrix6,readOnly = F)
  })
  
})


# Submit Beta value 
#
observeEvent(input$SubmequalvQValDisBI > 0,{
  AncDiscreteBI$vQ <- c(hot_to_r(input$matequalvQValDisBI)[1,])
})


# Set variance (steps) in for Q matrix
#
observeEvent(input$vQDisBI == 'vQValPerRatesDisBI', {
  AncDiscreteBI$vQ <- NULL
  AncDiscreteBI$matrix7 <- matrix(data = NA, nrow = 1, ncol = length(which(unique(c(AncDiscreteBI$modelMatrixBI)) != 0)),
                                  dimnames = list('variances',  sort(unique(c(AncDiscreteBI$modelMatrixBI))[which(unique(c(AncDiscreteBI$modelMatrixBI)) != 0)])))
  AncDiscreteBI$matrix7[1,] <- as.numeric(1)
  
  output$matvQValPerRatesDisBI <- renderRHandsontable( {# add matrix for a new Qmatrix
    rhandsontable(AncDiscreteBI$matrix7,readOnly = F)
  })
  
})


# Submit Beta values per rates
#
observeEvent(input$SubmbvQValPerRatesDisBI > 0,{
  AncDiscreteBI$vQ <- c(hot_to_r(input$matvQValPerRatesDisBI)[1,])
  
})

# Temporal object to print in info panel
# info: variances in steps for Qmatrix 
#
observeEvent(!is.null(AncDiscreteBI$vQ),{
  AncDiscreteBI$objectDiscreteBI <- AncDiscreteBI$vQ
})








#RUN

observeEvent(input$RunAnalyDisBI > 0,{
  
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {

                 incProgress(1/2)
  
  if(input$mcmcParDisBI == 'costummcmcDisBI') {
    AncDiscreteBI$outputDisBI <- make.simmap(tree = treeDisBI(),
                                             x = AncDiscreteBI$setCharacter, 
                                             model = AncDiscreteBI$modelMatrixBI,
                                             nsim = AncDiscreteBI$nsim,
                                             Q = AncDiscreteBI$QmatrixPar,
                                             pi = AncDiscreteBI$PiprobBI,
                                             samplefreq = AncDiscreteBI$sampleFreq,
                                             burnin = ceiling(AncDiscreteBI$burnin),
                                             prior = AncDiscreteBI$Priorpar,
                                             vQ= AncDiscreteBI$vQ)
  }else{
    AncDiscreteBI$outputDisBI <- make.simmap(tree = treeDisBI(),
                                             x = AncDiscreteBI$setCharacter, 
                                             model = AncDiscreteBI$modelMatrixBI,
                                             nsim = AncDiscreteBI$nsim,
                                             Q = AncDiscreteBI$QmatrixPar,
                                             pi = "fitzjohn",#AncDiscreteBI$PiprobBI,
                                             samplefreq = AncDiscreteBI$sampleFreq,
                                             burnin = ceiling(AncDiscreteBI$burnin))
  }
  
  

                 incProgress(2/2)


               })

  

})

# Temporal object to print in info panel
# info: Pi probabilities in the root
#
observeEvent(!is.null(AncDiscreteBI$outputDisBI),{
  AncDiscreteBI$objectDiscreteBI <- summary(AncDiscreteBI$outputDisBI)
})




observeEvent(input$RunAnalyDisBI,{

  Charlevels <- as.matrix(paste('PP of being', levels(AncDiscreteBI$setCharacter)))
  listNames <- lapply(levels(AncDiscreteBI$setCharacter),c)
  names(listNames) <- c(Charlevels[,1])
  
   updateSelectInput(session, "plotModelDisBI", choices = c('PP by node' = 'pieBI',listNames), selected = 'pieBI' )

   AncDiscreteBI$Density <- density(AncDiscreteBI$outputDisBI)
   
   updateSelectInput(session, "ploHPDDisBI", choices = c(names(AncDiscreteBI$Density$hpd) ))
   
   

   
   })


# PLOTS
# 

#Plot: Phylogenies, initial and outcomes

output$PhyloPlot10 <- renderPlot({
 
  
  if (input$RunAnalyDisBI > 0) {
    if (  input$plotModelDisBI %in%  levels(AncDiscreteBI$setCharacter)) {
      levelstab <- levels(AncDiscreteBI$setCharacter)
      
       wichPlotBI <- which(levelstab == as.character(input$plotModelDisBI))
     
      merged <- mergeMappedStates(AncDiscreteBI$outputDisBI, levelstab[levelstab != levelstab[wichPlotBI]], paste("not-",levelstab[wichPlotBI],sep = ''))
      
      densities <- density(merged, method = "densityMap",states = c(paste("not-",levelstab[wichPlotBI],sep = ''), as.character(levelstab[wichPlotBI])))
      
       plot(densities,fsize = c(0.4,0.8),lwd = c(3,6))

    }else{
      object <- summary(AncDiscreteBI$outputDisBI)
      DisColPal <- colorRampPalette(c("#02b2ce","#ffd004",  "#e52920"))(length(levels(AncDiscreteBI$setCharacter)))
      DisCols <- setNames(DisColPal,levels(AncDiscreteBI$setCharacter))
      
      plot(object,colors = DisCols, fsize = 0.7,ftype = "i")
      legend('topright',legend = levels(DataDisBI()),pch = 22,pt.cex = 1.5, pt.bg = DisCols, bty='n',cex = 0.8)
    }
    

  }else {
    DisColPal <- colorRampPalette(c("#02b2ce","#ffd004",  "#e52920"))(length(levels(DataDisBI())))

    DisCols <- setNames(DisColPal,levels(DataDisBI()))

    plotTree.datamatrix(treeDisBI(),as.data.frame(AncDiscreteBI$setCharacter),colors = list(DisCols),header=FALSE,fsize=0.45)

    legend('topright',legend = levels(DataDisBI()),pch = 22,pt.cex=1.5, pt.bg = DisCols, bty='n',cex = 0.8)
  }
})


# Model

output$PhyloPlot11 <- renderPlot({
  
  if(input$SubmQmatBI > 0){
    AncDiscreteBI$QmatrixPar
    
    matModelBI <- AncDiscreteBI$QmatrixPar
    class(matModelBI) <- "Qmatrix"
    plot(as.Qmatrix(matModelBI), main = 'User Q matrix', show.zeros = FALSE)
  }else{
  
  if (input$RunAnalyDisBI > 0) {
    matModelBI <- AncDiscreteBI$outputDisBI[[1]]$Q
    class(matModelBI) <- "Qmatrix"
    plot(as.Qmatrix(matModelBI), main = input$ModelsDisBI, show.zeros = FALSE)
      
  } else {
    if (!is.null(AncDiscreteBI$modelMatrixBI)) {
    matModelBI <- AncDiscreteBI$modelMatrixBI
    class(matModelBI) <- "Qmatrix"
    plot(as.Qmatrix(matModelBI), main = input$ModelsDisBI, show.zeros = FALSE)
    }
  }
    }

})

# Density

  output$PhyloPlot12 <- renderPlot({
if(input$RunAnalyDisBI > 0){
    if ( input$ploHPDDisBI %in% names(AncDiscreteBI$Density$hpd)) {
      
      plot(AncDiscreteBI$Density, transition = input$ploHPDDisBI[1])
}}
    
  })


