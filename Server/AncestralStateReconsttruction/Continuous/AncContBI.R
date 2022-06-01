##############################################################################
#     Continuous Character : Stochastic Mapping
##############################################################################

#Render print in Info panel: BI Analysis
#
output$infoPanelContinuousBI <- renderPrint({
  if (!is.null(AncContBI$objectContinuousBI)) {
    print(AncContBI$objectContinuousBI)
  }
})


# Temporal value
#
AncContBI <- reactiveValues()
AncContBI$objectContinuousBI <- NULL
AncContBI$pp <- NULL
AncContBI$pot <-NULL
AncContBI$setCharacterContBI <- NULL

#Tree

treeContBI <- eventReactive(c( treeInput()),{
  validate(
    need(try(treeInput()), "Please select a tree")
  )
  treeInput()
}
)


#Tree

DataContBI <- eventReactive(c( SelectedVar()),{
  validate(
    need(try(c(SelectedVar())), "Please select a data set")
  )
  SelectedVar()
}
)





# set character
observeEvent(DataContBI(),{
  
  AncContBI$setCharacterContBI <- setNames(DataContBI(),row.names(CharInput()))
})

#Covariance matrix 
#
phyloVCV <- eventReactive(DataContBI(), {
  phyl.vcv(as.matrix(AncContBI$setCharacterContBI),vcv(treeContBI()),1)
})


#Set parameters for mcmc run
costmat <- reactiveValues()
observeEvent(phyloVCV(), {
  costmat$cost <- list()
  costmat$cost$sig2 <- phyloVCV()$R[1, 1]
  costmat$cost$a <- phyloVCV()$alpha
  costmat$cost$y <- rep(costmat$cost$a, treeContBI()$Nnode - 1)
  costmat$cost$pr.mean <- c(1000,rep(0, treeContBI()$Nnode))
  costmat$cost$pr.var <- c(costmat$cost$pr.mean[1]^2, rep(1000, treeContBI()$Nnode))
  costmat$cost$prop <- rep(0.01 * max(phyloVCV()$C) * costmat$cost$sig2, treeContBI()$Nnode + 1)
  costmat$cost$sample <- 100
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
          AncContBI$mcmcBI <-      anc.Bayes(tree = treeContBI(),
                               x = AncContBI$setCharacterContBI,
                               ngen = as.numeric(input$ngenBI[1]))
                   } else {
                     AncContBI$mcmcBI <-  anc.Bayes(tree = treeContBI(),
                               x = AncContBI$setCharacterContBI,
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




# initial and output tree / phenogram if it is chosen

heightContBI <- reactive(input$PlotHeightContBI[1])
widthContBI <- reactive(input$PlotWidthContBI[1])




#Plot stochastic mapping state output

output$PhyloPlot4 <- renderPlot(height = heightContBI  , width = widthContBI,{
  req(treeContBI())
  if (input$runAncBI == 1) {
    if(input$phenogramBI == 1){
      matrix1 <- AncContBI$mcmcBI$mcmc[floor(0.2 * nrow(AncContBI$mcmcBI$mcmc)):nrow(AncContBI$mcmcBI$mcmc), c(4:length(colnames(AncContBI$mcmcBI$mcmc)) - 1)]
      estimates <- colMeans(matrix1)
      hpd <- matrix(NA,length(colnames(matrix1)), 2) #Estimate intervals todrw in phenogram
      for (i in 1:length(colnames(matrix1))) {
        class(matrix1[, i]) <- 'mcmc'
        hpd[i, 1] <- HPDinterval(matrix1[,i])[1,1]
        hpd[i, 2] <- HPDinterval(matrix1[,i])[1,2]
      }
      par(adj=0.5,
          cex.axis=0.9,
          cex.lab= 1,
          cex.main= 1.2,
          family="sans")
      
      tree <- paintSubTree(tree = as.phylo(treeContBI()), 
                           node = length(treeContBI()$tip) + 1, "1")
      trans <- as.character(floor(0:26/2)) #make transparent lines
      trans[as.numeric(trans) < 10] <- paste("0", trans[as.numeric(trans) < 10], sep = "")
      for (i in 0:50) { #Plot every line or interval
        p <- i/length(trans)
        phenogram(tree = as.phylo(tree), x = c(AncContBI$setCharacterContBI,(1 - p) * hpd[, 1] + p * estimates),
                  colors = make.transparent("mediumpurple", 0.05), add = i > 0, ftype = 'off')
        phenogram(tree = as.phylo(tree), x = c(AncContBI$setCharacterContBI, (1 - p) * hpd[, 2] + p * estimates),
                  colors = make.transparent("mediumpurple", 0.05), add = TRUE)
      }
      phenogram(tree = as.phylo(tree), x = c(AncContBI$setCharacterContBI, estimates),
                add = TRUE, colors = "white",lwd= 1.5) # The mean 
    }else{
  plot5 <- contMap(tree = treeContBI(),
                   x = AncContBI$setCharacterContBI,
                   method = 'user', show.tip.label = T,
                   anc.states = estimatesBI(),
                  plot = FALSE
                  )
  n <- length(plot5$cols)
  plot5$cols[1:n] <- colorRampPalette(c("#02b2ce","#ffd004",  "#e52920"), bias=1.5)(n)
  plot(plot5, outline = F,lwd =2.5,fsize = input$tipSizeContBI[1])

  AncContBI$pp <- get(x = "last_plot.phylo", envir = .PlotPhyloEnv)
  AncContBI$pot <-as.data.frame(cbind('Node'=c(1:AncContBI$pp$Ntip,AncContBI$pp$Ntip+1:AncContBI$pp$Nnode), 'x' = AncContBI$pp$xx, 'y' = AncContBI$pp$yy))
  names(AncContBI$pot) <-c('Node','x','y')
  
  points(x = AncContBI$pp$xx[c(AncContBI$pp$Ntip+1:AncContBI$pp$Nnode)], y = AncContBI$pp$yy[c(AncContBI$pp$Ntip+1:AncContBI$pp$Nnode)],
         pch = 21, bg = "white", cex = 2.3)
  nodelabels(bg = "white", cex = 0.6, frame = 'none')
  
    }
  
  }else {

  plot.phylo(x = treeContBI(),
             show.tip.label = T,
             cex = input$tipSizeContBI[1],
             use.edge.length = T,edge.width = 0.8,edge.color = 'grey40')
  
}
  })






#Temporal object to print in info panel



#update nodes
observeEvent(!is.null(AncContBI$mcmcBI),{
  updateSelectInput(session, "plotNodesBI",
                    choices = names(AncContBI$mcmcBI$mcmc)[c(2,length(names(AncContBI$mcmcBI$mcmc)))],
                    selected = 'logLik')
})

#update nodes
observeEvent(c(input$phenogramBI==0, input$plot_click, input$runAncBI),{
  #!is.null(AncContBI$mcmcBI)
  updateSelectInput(session, "plotNodesBI",
                    choices = c(input$plotNodesBI,nearPoints(AncContBI$pot, input$plot_click, xvar = 'x', yvar = 'y')[1,1]),
                    selected = c(input$plotNodesBI,nearPoints(AncContBI$pot, input$plot_click, xvar = 'x', yvar = 'y')[1,1]))
})



#Temporal object to print in info panel
#info: Plotting nodes
#
observeEvent(input$plotNodesBI,{
  AncContBI$objectContinuousBI <- c('Ploting posterior probabilities for' , paste('node:',input$plotNodesBI) )
})


#Plot posterior probabilities per node (trace mcmc plot)
#
output$trace1 <- renderPlot( {
  if (!is.null(input$plotNodesBI)) {
   # if (length(input$plotNodesBI) > 1) {
      pd <- AncContBI$mcmcBI$mcmc[, c(1,which(colnames(AncContBI$mcmcBI$mcmc) %in% input$plotNodesBI))]
      xlim <- range(pd$gen)
      ylim <- range(pd[, 2:length(pd)])
      par(adj=0.5,
          cex.axis=0.9,
          cex.lab= 1,
          cex.main= 1.2,
          family="sans")
      plot(NA, xlim = xlim, ylim = ylim, bty = "n", ylab = "density", xlab = input$dataVar, las = 1,axes = F)
      clip(xlim[1], xlim[2], ylim[1], ylim[2])
      grid( lty = 2, lwd = 0.3)
      axis(side = 1, lwd=0.5, lwd.ticks = .5,mgp = c(3, 1, 0))
      axis(side = 2, lwd=0.5,lwd.ticks = .5,las = 2, mgp = c(3, 1, 0.5))
      cols <- colorRampPalette(colors = c("mediumpurple","red"))(length(1:length(pd)))
      for (i in 2:length(pd)) {
        lines(x = pd$gen, y = pd[,i], col = make.transparent(cols[i], 0.5))
      }
     # } else {
      #   plot.anc.Bayes(x = AncContBI$mcmcBI,
      #                  bty = "l",
      #                  main = "Likelihood-profile from MCMC",
      #                  font.main = 3,
      #                  what = input$plotNodesBI[1])
      # }
    }
  })


#Plot  posterior probabilities per node (Density mcmc plot)
#
output$desnsity1 <- renderPlot( {
  if (!is.null(input$plotNodesBI)) {
   # if (length(input$plotNodesBI) > 1) {
      pd <- lapply(input$plotNodesBI,function(nn,mcmc) density(mcmc, what = nn), mcmc = AncContBI$mcmcBI)
      xlim <- exp(range(sapply(pd,function(dd) range(dd$x))))
      ylim <- range(sapply(pd, function(dd) range(dd$y)))
      par(adj=0.5,
          cex.axis=0.9,
          cex.lab= 1,
          cex.main= 1.2,
          family="sans")
      plot(NA, xlim = xlim, ylim = ylim, bty = "n", ylab = "density", xlab = input$dataVar,las = 1,axes = F, log = "x")
      clip(x1 = xlim[1], x2 = xlim[2], y1 = ylim[1], y2 = ylim[2])
      grid( lty = 2, lwd = 0.3)
      axis(side = 1, lwd=0.5, lwd.ticks = .5,mgp = c(3, 1, 0))
      axis(side = 2, lwd=0.5,lwd.ticks = .5,las = 2, mgp = c(3, 1, 0.5))
      cols <- colorRampPalette(colors = c("mediumpurple","red"))(length(input$plotNodesBI))
      nulo <- mapply(function(dd,col) polygon(exp(dd$x),dd$y,col = make.transparent(col, 0.2), border = col),
                     dd = pd, col = cols)
      # } else {
      #   plot(density.anc.Bayes(x = AncContBI$mcmcBI, what = input$plotNodesBI[1]), bty = "l", main = "Posterior density", font.main = 3)
      # }
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

