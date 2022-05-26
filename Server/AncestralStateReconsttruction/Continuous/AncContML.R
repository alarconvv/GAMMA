##############################################################################
#     Continuous Character : Maximum likelihood
##############################################################################


# Temporal value
#
AncContMl <- reactiveValues()
AncContMl$objetContinuousML <- NULL
AncContMl$Models <- NULL


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
  if (!is.null(AncContMl$objetContinuousML)) {
    print(AncContMl$objetContinuousML)
    }
  })



#Plot initial tree if models do not have been estimated, otherwise plot model output
#

heightContMl <- reactive(input$PlotHeightContMl[1])
widthContMl <- reactive(input$PlotWidthContMl[1])

output$PhyloPlot2 <- renderPlot(height = heightContMl  , width = widthContMl, {
  if(input$runAncML >0){
  if (input$phenogramML == 1) {
    if (input$mapModelMl == 'BM') {
      phenogram(tree = treeInput(), #as.phylo method to plot in black color
                x =  c(setNames(transform(), row.names(CharInput())),AncContMl$Models$BM$ace),
                spread.cost = c(1,0), 
                fsize = input$tipSizeContMl[1],
                lwd=0.8,colors ='grey40' )
      
    } else if (input$mapModelMl == 'EB') {
      phenogram(tree = treeInput(), #as.phylo method to plot in black color
                x =  c(setNames(transform(), row.names(CharInput())),AncContMl$Models$EB$ace),
                spread.cost = c(1,0), 
                fsize = input$tipSizeContMl[1],
                lwd=0.8,colors ='grey40')
    } else if (input$mapModelMl == 'OU') {
      phenogram(tree = treeInput(), #as.phylo method to plot in black color
                x =  c(setNames(transform(), row.names(CharInput())),AncContMl$Models$OU$ace),
                spread.cost = c(1,0), 
                fsize = input$tipSizeContMl[1],
                lwd=0.8,colors ='grey40')
    }
  }else{
  
  if (input$mapModelMl == 'BM') {
    plotBM <- contMap(tree = treeInput(),
                      x = setNames(transform(), row.names(CharInput())),
                      method = 'user',
                      anc.states = c(setNames(transform(), row.names(CharInput())),AncContMl$Models$BM$ace),
                      show.tip.label = T,
                      plot = FALSE
                      )
    n <- length(plotBM$cols)
    plotBM$cols[1:n] <- paletteer::paletteer_c("grDevices::Purple-Yellow", n)
    plot(plotBM, outline = F,lwd =2,fsize = input$tipSizeContMl[1])
  } else if (input$mapModelMl == 'EB') {
    plotEB <- contMap(tree = treeInput(), 
                      x = setNames(transform(),row.names(CharInput())),
                      method = 'user',
                      anc.states = c(setNames(transform(),row.names(CharInput())),AncContMl$Models$EB$ace),
                      show.tip.label = T,
                      plot = FALSE)
    n <- length(plotEB$cols)
    plotEB$cols[1:n] <- paletteer::paletteer_c("grDevices::Purple-Yellow", n)
    plot(plotEB, outline = F,lwd =2,fsize = input$tipSizeContMl[1])
  } else if (input$mapModelMl == 'OU') {
    plotOU <- contMap(tree = treeInput(), 
                      x = setNames(transform(),row.names(CharInput())),
                      method = 'user',
                      anc.states = c(setNames(transform(),row.names(CharInput())),AncContMl$Models$OU$ace),
                      show.tip.label = T,
                      plot = FALSE)
    n <- length(plotOU$cols)
    plotOU$cols[1:n] <- paletteer::paletteer_c("grDevices::Purple-Yellow", n)
    plot(plotOU, outline = F,lwd =2,fsize = input$tipSizeContMl[1])
  }
    } 
  }else {
      plot.phylo(x = treeInput(),
                 show.tip.label = T,
                 cex = input$tipSizeContMl[1],
                 use.edge.length = T,edge.width = 0.8,edge.color = 'grey40')
      }
  })


#Plot histogram or frequency distribution
#
output$histo1 <- renderPlot( {
  
  colors1 <-col2rgb(paletteer::paletteer_c("grDevices::Purple-Yellow", 1))
  color2 <- rgb(red = colors1[1,]/255,green =colors1[2,]/255,blue = colors1[3,]/255,alpha = 0.5)
  
  if (input$AncPIC == 1) { #if PIC is required just for visualized, it is not taken into account for analysing
   
    
    h <- hist(x = pic(x = transform(), phy = treeInput()),plot=F)
    
    par(
        cex.axis=0.7,
        cex.lab= 0.9,
        cex.main= 1,
        family="sans"
    )
    plot(h$mids, h$counts, yaxs = "i",
         type = 'n', bty = 'n',axes = F,main= 'Histogram: Phylogenetic Independent Contrasts', sub = NULL, xlab= input$dataVar, ylab= ' N Tips' )
    grid(10,10, lty = 2, lwd = 0.3)
    axis(side = 1,xlim = range(h$mids)*1.1, lwd=0, lwd.ticks = .5)
    axis(side = 2,ylim = c(0, max(h$counts)), lwd=0,lwd.ticks = .5)
    hist(x = pic(x = transform(), phy = treeInput()), add = TRUE, border= F,
         col=color2)
    box(lwd= 0.5, col= 'grey')
    
  
  } else {
    
    h <- hist(x = transform(),plot=F)
    par(
        cex.axis=0.7,
        cex.lab= 0.9,
        cex.main= 1,
        family="sans"
    )
    plot(h$mids, h$counts, yaxs = "i",
         type = 'n', bty = 'n',axes = F, main='Histogram: raw data', sub = NULL, xlab= input$dataVar, ylab= ' N Tips')
    grid(10,10, lty = 2, lwd = 0.3)
    axis(side = 1,xlim = range(h$mids)*1.1, lwd=0, lwd.ticks = .5)
    axis(side = 2,ylim = c(0, max(h$counts)), lwd=0,lwd.ticks = .5)
    hist(x = transform(), add = TRUE, border= F,
         col= color2)
    box(lwd= 0.5, col= 'grey')
  }
})


# Plot QQ plot for fitting to a normal distribution
# 
output$QQ1 <- renderPlot( {
  colors1 <-col2rgb(paletteer::paletteer_c("grDevices::Purple-Yellow", 1))
  color2 <- rgb(red = colors1[1,]/255,green =colors1[2,]/255,blue = colors1[3,]/255,alpha = 0.5)
  color3 <- rgb(red = colors1[1,]/255,green =colors1[2,]/255,blue = colors1[3,]/255)
  
  if (input$AncPIC == 1) {
    q <-qqnorm(y = pic(x = transform(), phy = treeInput()),plot=F)
    par(adj=0.5,
        cex.axis=0.7,
        cex.lab= 0.9,
        cex.main= 1,
        family="sans"
    )
    
    plot(q$x, q$y, yaxs = "i", 
         type = 'p',axes = F,main = 'Normal Q-Q Plot: PIC', sub = NULL, xlab = 'Theoretical Quantiles', ylab = 'Tip Quantiles',
         col=color3,pch=21,bg=color2)
    grid(10,10, lty = 2, lwd = 0.3)
    axis(side = 1,xlim = range(q$x)*1.1,  lwd=0, lwd.ticks = .5)
    axis(side = 2,ylim = c(0, max(q$y)), lwd=0,lwd.ticks = .5)
    box(lwd= 0.5, col= 'grey5')
  } else {
    q <-qqnorm(y = transform(),plot=F)
    par(adj=0.5,
        cex.axis=0.7,
        cex.lab= 0.9,
        cex.main= 1,
        family="sans"
    )
    
    plot(q$x, q$y, yaxs = "i", 
         type = 'p',axes = F,main = 'Normal Q-Q Plot: PIC', sub = NULL, xlab = 'Theoretical Quantiles', ylab = 'Tip Quantiles',
         col=color3,pch=21,bg=color2)
    grid(10,10, lty = 2, lwd = 0.3)
    axis(side = 1,xlim = range(q$x)*1.1,  lwd=0, lwd.ticks = .5)
    axis(side = 2,ylim = c(0, max(q$y)), lwd=0,lwd.ticks = .5)
    box(lwd= 0.5, col= 'grey')
  }
})

#Fit models: Run Analysis
#
observeEvent(input$runAncML, {

  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
               
                 incProgress(1/2)
            
  
  
  if ( "BM" %in% input$ModelContinuous ) {
    AncContMl$Models$BM <- anc.ML(tree = treeInput(), 
                          x = setNames(transform(),row.names(CharInput())), 
                          maxit = input$maxitBM[1], 
                          model = 'BM')
  }
  
  if ("EB" %in% input$ModelContinuous) { 
    AncContMl$Models$EB <- anc.ML(tree = treeInput(),
                          x =  setNames(transform(),row.names(CharInput())), 
                          maxit = input$maxitEB[1], 
                          model = 'EB')
  }
  
  if ("OU" %in% input$ModelContinuous) {
    AncContMl$Models$OU <- anc.ML(tree = treeInput(),
                          x = setNames(transform(),row.names(CharInput())),
                          maxit = input$maxitOU[1],
                          model = 'OU')
  }
                 
                 incProgress(2/2)
                 
                 
               })
  
})






# update  Selection input to plot an specific model that had been run
# 
observeEvent(input$runAncML, {
  updateSelectInput(session = session, inputId = "mapModelMl", choices = names(AncContMl$Models))
})


# Temporal object to print in info panel
# info: output per every model run
#
observeEvent(input$runAncML, {
if(!is.null(AncContMl$Models)){
  AncContMl$objetContinuousML <- AncContMl$Models
}
})


#Calculate AIC (No keep it, just show it)
#
observeEvent(input$MLModAIC,{
  if(!is.null(AncContMl$Models)){
  AncContMl$objetContinuousML <- sapply(AncContMl$Models, AIC)}
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
      utils::capture.output(AncContMl$Models,file = file)
      } else {
        saveRDS(object = AncContMl$Models,file = file)
        }
    })

