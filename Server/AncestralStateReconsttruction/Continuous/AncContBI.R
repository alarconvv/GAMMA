##############################################################################
#     Continuous Character : Stochastic Mapping
##############################################################################

# Temporal value
#
AncContBI <- reactiveValues()
AncContBI$objectContinuousBI <- NULL


#Covariance matrix 
#
phyloVCV <- eventReactive(SelectedVar(), {
  phyl.vcv(as.matrix(setNames(SelectedVar(),row.names(CharInput()))),vcv(treeInput()),1)
})


#Set parameters for mcmc run
costmat <- reactiveValues()
observeEvent(phyloVCV(), {
  costmat$cost <- list()
  costmat$cost$sig2 <- phyloVCV()$R[1, 1]
  costmat$cost$a <- phyloVCV()$alpha
  costmat$cost$y <- rep(costmat$cost$a, treeInput()$Nnode - 1)
  costmat$cost$pr.mean <- c(1000,rep(0, treeInput()$Nnode))
  costmat$cost$pr.var <- c(costmat$cost$pr.mean[1]^2, rep(1000, treeInput()$Nnode))
  costmat$cost$prop <- rep(0.01 * max(phyloVCV()$C) * costmat$cost$sig2, treeInput()$Nnode + 1)
  costmat$cost$sample <- 100
  })


#Render print in Info panel: BI Analysis
#
output$infoPanelContinuousBI <- renderPrint({
  if (!is.null(AncContBI$objectContinuousBI)) {
    print(AncContBI$objectContinuousBI)
  }
})


#Temporal object ro print in info panel
#info: print setting matrix
observeEvent(input$parametersBI != '"select', {
  AncContBI$objectContinuousBI <- costmat$cost
  })


#Update Text input if user wants costumize the mcmc analysis
#
observeEvent(input$parametersBI == 'costumizeBI', {
  updateTextInput(session, "sig2BI", value = costmat$cost$sig2)
  updateTextInput(session, "aBI", value = costmat$cost$a)
  updateTextInput(session, "sampleBI", value = costmat$cost$sample)
  })

#Assign values for a costumized mcmc run
#
observeEvent(input$parametersBI == 'costumizeBI', {
  if (input$sig2BI == 1) {
    costmat$cost$sig2[1] <- as.numeric(input$sig2BI[1])
    } else if (input$aBI == 1) {
      costmat$cost$a[1] <- as.numeric(input$aBI[1])
  } else if (input$sampleBI == 1) {
    costmat$cost$sample[1] <- as.numeric(input$sampleBI[1])
    }
  })


# Run mcmc analysis

observeEvent(input$runAncBI,{
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 incProgress(1/2)
              
                   if (input$parametersBI == 'defaultBI') {
          AncContBI$mcmcBI <-      anc.Bayes(tree = treeInput(),
                               x = setNames(transform(),row.names(CharInput())),
                               ngen = as.numeric(input$ngenBI[1]))
                   } else {
                     AncContBI$mcmcBI <-  anc.Bayes(tree = treeInput(),
                               x = setNames(transform(),row.names(CharInput())),
                               ngen = as.numeric(input$ngenBI[1]),
                               control = costmat$cost)
                   }
                   
                 incProgress(2/2)
               })
})


#Get mean from posterior probabilities per node
estimatesBI <- eventReactive(AncContBI$mcmcBI, {
  colMeans(AncContBI$mcmcBI$mcmc[floor(as.numeric(input$BurninBI[1]) * nrow(AncContBI$mcmcBI$mcmc)):nrow(AncContBI$mcmcBI$mcmc), ])
  })

#Temporal object to print in info panel
#info: Posterior probabilities means
observeEvent(!is.null(AncContBI$mcmcBI),{
  AncContBI$objectContinuousBI <-  estimatesBI() ## estimates
})


#update nodes
observeEvent(!is.null(AncContBI$mcmcBI),{
  updateSelectInput(session, "plotNodesBI",
                    choices = names(AncContBI$mcmcBI$mcmc),
                    selected = 'logLik')
  })


#Temporal object to print in info panel
#info: Plotting nodes
#
observeEvent(!is.null(names(AncContBI$mcmcBI$mcmc)),{
  AncContBI$objectContinuousBI <- paste('Right plots: nodes', input$plotNodesBI, collapse = '')
})


# initial and output tree / phenogram if it is chosen

heightContBI <- reactive(input$PlotHeightContBI[1])
widthContBI <- reactive(input$PlotWidthContBI[1])

output$PhyloPlot5 <- renderPlot( {
  if (input$runAncBI == 1) {
    #if (!input$phenogramBI == 1) {
      # if (any(grepl('[0-9]', input$plotNodesBI) == T)) {
      #   nodesBI4 <- input$plotNodesBI[which(grepl('[0-9]', input$plotNodesBI) == T)]
      #   plot.phylo(x = treeInput(), 
      #              show.tip.label = T,
      #              cex = 0.1,
      #              use.edge.length = T)
      #   pp <- get(x = "last_plot.phylo", envir = .PlotPhyloEnv)
      #   cols <- colorRampPalette(colors = c("blue","red"))(length(2:length(input$plotNodesBI)))
      #   points(x = pp$xx[c(as.numeric(nodesBI4))], y = pp$yy[c(as.numeric(nodesBI4))],
      #          pch = 21, col = cols, bg = "white", cex = 1.5)
      #   nodelabels(bg = "white", cex = 0.6, frame = 'none', node = c(as.numeric(nodesBI4)))
      #   points(x = pp$xx[c(as.numeric(nodesBI4))], y = pp$yy[c(as.numeric(nodesBI4))],
      #          pch = 21, col = cols, bg = sapply(cols,make.transparent,0.2), cex = 1.5)
      #   } else {
      #     plot.phylo(x = treeInput(), 
      #                show.tip.label = input$tipLabels[1],
      #                cex = input$tipSize[1], 
      #                use.edge.length = input$branchLength[1])
      #     nodelabels(bg = "white", cex = 0.3, frame = "circle")
      #  }

      
     # } else {
        matrix1 <- AncContBI$mcmcBI$mcmc[floor(0.2 * nrow(AncContBI$mcmcBI$mcmc)):nrow(AncContBI$mcmcBI$mcmc), c(4:length(colnames(AncContBI$mcmcBI$mcmc)) - 1)]
        estimates <- colMeans(matrix1)
        hpd <- matrix(NA,length(colnames(matrix1)), 2) #Estimate intervals todrw in phenogram
      for (i in 1:length(colnames(matrix1))) {
        class(matrix1[, i]) <- 'mcmc'
        hpd[i, 1] <- HPDinterval(matrix1[,i])[1,1]
        hpd[i, 2] <- HPDinterval(matrix1[,i])[1,2]
      }
      tree <- paintSubTree(tree = as.phylo(treeInput()), 
                           node = length(treeInput()$tip) + 1, "1")
      trans <- as.character(floor(0:26/2)) #make transparent lines
      trans[as.numeric(trans) < 10] <- paste("0", trans[as.numeric(trans) < 10], sep = "")
      for (i in 0:50) { #Plot every line or interval
        p <- i/length(trans)
        phenogram(tree = as.phylo(tree), x = c(setNames(transform(),row.names(CharInput())),(1 - p) * hpd[, 1] + p * estimates),
                  colors = make.transparent("#0000ff", 0.1), add = i > 0, ftype = 'off')
        phenogram(tree = as.phylo(tree), x = c(setNames(transform(),row.names(CharInput())), (1 - p) * hpd[, 2] + p * estimates),
                  colors = make.transparent("#0000ff", 0.1), add = TRUE)
        }
      phenogram(tree = as.phylo(tree), x = c(setNames(transform(),row.names(CharInput())), estimates),
                add = TRUE, colors = "white") # The mean 
     # }
  }
    # } else {
    #   plot.phylo(x = treeInput(),
    #              show.tip.label = T,
    #              cex = input$tipSizeContBI[1],
    #              use.edge.length = T,edge.width = 0.8,edge.color = 'grey40')
    # 
    #   }
  })


#Plot stochastic mapping state output

output$PhyloPlot4 <- renderPlot(height = heightContBI  , width = widthContBI,{
  if (input$runAncBI == 1) {
  plot5 <- contMap(tree = treeInput(),
                   x = setNames(transform(),row.names(CharInput())),
                   method = 'user', show.tip.label = input$tipLabelsML[1],
                   anc.states = estimatesBI(),
                  plot = FALSE
                  )
  n <- length(plot5$cols)
  plot5$cols[1:n] <- paletteer::paletteer_c("grDevices::Purple-Yellow", n)
  plot(plot5, outline = F,lwd =2,fsize = input$tipSizeContMl[1])
  pp <- get(x = "last_plot.phylo", envir = .PlotPhyloEnv)
  points(x = pp$xx, y = pp$yy,
         pch = 21, bg = "white", cex = 1.5)
  nodelabels(bg = "white", cex = 0.6, frame = 'none')
  AncContBI$pot <-as.data.frame(cbind('Node'=c(1:pp$Ntip,pp$Ntip+1:pp$Nnode), 'x' = pp$xx, 'y' = pp$yy))
  names(AncContBI$pot) <-c('Node','x','y')
  }else{

  plot.phylo(x = treeInput(),
             show.tip.label = T,
             cex = input$tipSizeContBI[1],
             use.edge.length = T,edge.width = 0.8,edge.color = 'grey40')
}
  })






#Temporal object to print in info panel
#info: Plotting nodes
#
observeEvent(input$plot_click,{
  AncContBI$objectContinuousBI <- nearPoints(AncContBI$pot, input$plot_click, xvar = 'x', yvar = 'y')
})



#update nodes
observeEvent(!is.null(AncContBI$mcmcBI),{
  updateSelectInput(session, "plotNodesBI",
                    choices = names(AncContBI$mcmcBI$mcmc)[c(2,length(names(AncContBI$mcmcBI$mcmc)))],
                    selected = 'logLik')
})

#update nodes
observeEvent(input$plot_click,{
  #!is.null(AncContBI$mcmcBI)
  updateSelectInput(session, "plotNodesBI",
                    choices = c(input$plotNodesBI,nearPoints(AncContBI$pot, input$plot_click, xvar = 'x', yvar = 'y')[1,1]),
                    selected = c(input$plotNodesBI,nearPoints(AncContBI$pot, input$plot_click, xvar = 'x', yvar = 'y')[1,1]))
})

#Plot posterior probabilities per node (trace mcmc plot)
#
output$trace1 <- renderPlot( {
  if (!is.null(input$plotNodesBI)) {
    if (length(input$plotNodesBI) > 1) {
      pd <- AncContBI$mcmcBI$mcmc[, c(1,which(colnames(AncContBI$mcmcBI$mcmc) %in% input$plotNodesBI))]
      xlim <- range(pd$gen)
      ylim <- range(pd[, 2:length(pd)])
      par(mar = c(5.1,4.1,3.1,1.1))
      plot(NA, xlim = xlim, ylim = ylim, bty = "n", ylab = "density", xlab = "body mass (kg)", las = 1)
      clip(xlim[1], xlim[2], ylim[1], ylim[2])
      grid()
      cols <- colorRampPalette(colors = c("blue","red"))(length(1:length(pd)))
      for (i in 2:length(pd)) {
        lines(x = pd$gen, y = pd[,i], col = make.transparent(cols[i], 0.5))
        }
      } else {
        plot.anc.Bayes(x = AncContBI$mcmcBI,
                       bty = "l",
                       main = "Likelihood-profile from MCMC",
                       font.main = 3,
                       what = input$plotNodesBI[1])
      }
    }
  })


#Plot  posterior probabilities per node (Density mcmc plot)
#
output$desnsity1 <- renderPlot( {
  if (!is.null(input$plotNodesBI)) {
    if (length(input$plotNodesBI) > 1) {
      pd <- lapply(input$plotNodesBI,function(nn,mcmc) density(mcmc, what = nn), mcmc = AncContBI$mcmcBI)
      xlim <- exp(range(sapply(pd,function(dd) range(dd$x))))
      ylim <- range(sapply(pd, function(dd) range(dd$y)))
      par(mar = c(5.1,4.1,3.1,1.1))
      plot(NA, xlim = xlim, ylim = ylim, bty = "n", ylab = "density", xlab = "body mass (kg)", las = 1, log = "x")
      clip(x1 = xlim[1], x2 = xlim[2], y1 = ylim[1], y2 = ylim[2])
      grid()
      cols <- colorRampPalette(colors = c("blue","red"))(length(input$plotNodesBI))
      nulo <- mapply(function(dd,col) polygon(exp(dd$x),dd$y,col = make.transparent(col, 0.2), border = col),
                     dd = pd, col = cols)
      } else {
        plot(density.anc.Bayes(x = AncContBI$mcmcBI, what = input$plotNodesBI[1]), bty = "l", main = "Posterior density", font.main = 3)
      }
    }
  })


#Download mcmc output
#
output$downloadAncBI <- downloadHandler(
  filename = function(){
    if (input$exportAncBI == 'BIancTXT') {
      c('Out_Anc_BI.txt')
      }else{
        c('Out_Anc_BI.RDS')
        }
    },
  content = function(file){
    if (input$exportAncBI == 'BIancTXT') {
      utils::capture.output(AncContBI$mcmcBI$mcmc,file = file)
      } else {
        saveRDS(object = AncContBI$mcmcBI,file = file)
      }
    }
  )

