# Transform character
# 
transform <- eventReactive(input$transform1, {
  if (input$transform1 == 'ln') {
    log(SelectedVar())
  } else if (input$transform1 == 'log10') {
    log(SelectedVar(), 10)
  } else if (input$transform1 == 'squareRoot') {
    sqrt(SelectedVar())
  } else if (input$transform1 == 'cubeRoot') {
    SelectedVar()^(1/3)
  } else if (input$transform1 == 'Reciprocal') {
    1/SelectedVar()
  } else if (input$transform1 == 'exp1') {
    exp(SelectedVar())
  } else {SelectedVar()
  }
})


#Render print in Info panel: ML Analysis
#
output$infoPanelContinuousML <- renderPrint( {
  if (!is.null(v$objetContinuousML)) {
    print(v$objetContinuousML)
    }
  })

#Plot initial tree if models do not have been estimated, otherwise plot model output
#
output$PhyloPlot2 <- renderPlot( {
  if (input$mapModelMl == 'BM') {
    plotBM <- contMap(tree = treeInput(),
                      x = setNames(transform(), row.names(CharInput())),
                      method = 'user',
                      anc.states = c(setNames(transform(), row.names(CharInput())),v$Models$BM$ace),
                      show.tip.label = input$tipLabelsML[1],
                      fsize = 0.1,
                      plot = FALSE)
    n <- length(plotBM$cols)
    plotBM$cols[1:n] <- paletteer::paletteer_c("grDevices::Purple-Yellow", n)
    plot(plotBM, outline = F)
  } else if (input$mapModelMl == 'EB') {
    plotEB <- contMap(tree = treeInput(), 
                      x = setNames(transform(),row.names(CharInput())),
                      method = 'user',
                      anc.states = c(setNames(transform(),row.names(CharInput())),v$Models$EB$ace),
                      show.tip.label = input$tipLabelsML[1],
                      fsize = 0.1,
                      plot = FALSE)
    n <- length(plotEB$cols)
    plotEB$cols[1:n] <- paletteer::paletteer_c("grDevices::Purple-Yellow", n)
    plot(plotEB, outline = F)
  } else if (input$mapModelMl == 'OU') {
    plotOU <- contMap(tree = treeInput(), 
                      x = setNames(transform(),row.names(CharInput())),
                      method = 'user',
                      anc.states = c(setNames(transform(),row.names(CharInput())),v$Models$OU$ace),
                      show.tip.label = input$tipLabelsML[1],
                      fsize = 0.1,
                      plot = FALSE)
    n <- length(plotOU$cols)
    plotOU$cols[1:n] <- paletteer::paletteer_c("grDevices::Purple-Yellow", n)
    plot(plotOU, outline = F)
    } else {
      plot.phylo(x = treeInput(),
                 show.tip.label = input$tipLabels[1],
                 cex = input$tipSize[1],
                 use.edge.length = input$branchLength[1])
      #nodelabels(bg="white",cex=0.3,frame="circle")
      }
  })


#Plot initial Phenogram if models do not have been estimated, otherwise plot model output
#
output$PhyloPlot3 <- renderPlot( {
  phenogram(tree = as.phylo(treeInput()), #as.phylo method to plot in black color
            x = setNames(transform(),row.names(CharInput())), 
            spread.cost = c(1,0), 
            fsize = 0.7)
})


#Plot histogram or frequency distribution
#
output$histo1 <- renderPlot( {
  if (input$AncPIC == 1) { #if PIC is required just for visualized, it is not taken into account for analysing
    hist(x = pic(x = transform(), phy = treeInput()))
  } else {
    hist(x = transform())
  }
})


# Plot QQ plot for fitting to a normal distribution
# 
output$QQ1 <- renderPlot( {
  if (input$AncPIC == 1) {
    qqnorm(y = pic(x = transform(), phy = treeInput()))
  } else {
    qqnorm(y = transform())
  }
})

#Fit models: Run Analysis
#
observeEvent(input$runAncML == 1, {
  if ( "BM" %in% input$ModelContinuous ) {
    v$Models$BM <- anc.ML(tree = treeInput(), 
                          x = setNames(transform(),row.names(CharInput())), 
                          maxit = input$maxitBM[1], 
                          model = 'BM')
  }
  
  if ("EB" %in% input$ModelContinuous) { 
    v$Models$EB <- anc.ML(tree = treeInput(),
                          x =  setNames(transform(),row.names(CharInput())), 
                          maxit = input$maxitEB[1], 
                          model = 'EB')
  }
  
  if ("OU" %in% input$ModelContinuous) {
    v$Models$OU <- anc.ML(tree = treeInput(),
                          x = setNames(transform(),row.names(CharInput())),
                          maxit = input$maxitOU[1],
                          model = 'OU')
  }
})


# update  Selection input to plot an specific model that had been run
# 
observeEvent(input$runAncML == 1, {
  updateSelectInput(session = session, inputId = "mapModelMl", choices = names(v$Models))
})


# Temporal object to print in info panel
# info: output per every model run
#
observeEvent(input$runAncML == 1, {
  if (!is.null(v$Models)) {
    v$objetContinuousML <- v$Models
  }
})


#Calculate AIC (No keep it, just show it)
#
observeEvent(input$MLModAIC == 1,{
  v$objetContinuousML <- sapply(v$Models, AIC)
})


#Download output
#
output$downloadAnc <- downloadHandler(
  filename = function() {
    if (input$exportMLanc == 'MLancTXT') {
      c('Out_Anc_ML.txt')
    } else {
      c('Out_Anc_ML.RDS')
    }
  },
  content = function(file) {
    if (input$exportMLanc == 'MLancTXT') {
      utils::capture.output(v$Models,file = file)
      } else {
        saveRDS(object = v$Models,file = file)
        }
    })

