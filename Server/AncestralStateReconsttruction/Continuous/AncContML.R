##############################################################################
#     Continuous Character : Maximum likelihood
##############################################################################

#Render print in Info panel: ML Analysis
#
output$infoPanelContinuousML <- renderPrint( {
  if (!is.null(AncContMl$objetContinuousML)) {
    print(AncContMl$objetContinuousML)
  }
})




# Temporal value
#
AncContMl <- reactiveValues()
AncContMl$objetContinuousML <- NULL
AncContMl$Models <- NULL
AncContMl$setCharacterContMl <-NULL
AncContMl$transform1 <-NULL
AncContMl$transform2 <-NULL


#Tree

treeContML <- eventReactive(c( treeInput()),{
  validate(
    need(try(c(treeInput())), "Please select a tree")
  )
  treeInput()
}
)

#Data

DataContML <- eventReactive(c( SelectedVar()),{
  validate(
    need(try(c(SelectedVar())), "Please select a data set")
  )
  SelectedVar()
}
)






# Transform character
# 
transform <- eventReactive(c(DataContML(),input$transform1), {
  
  AncContMl$transform1 <- input$transform1[1]
  
  if (input$transform1 == 'ln') {
    log(DataContML())
  } else if (input$transform1 == 'log10') {
    log(DataContML(), 10)
  } else if (input$transform1 == 'squareRoot') {
    sqrt(DataContML())
  } else if (input$transform1 == 'cubeRoot') {
    DataContML()^(1/3)
  } else if (input$transform1 == 'Reciprocal') {
    1/DataContML()
  } else if (input$transform1 == 'exp1') {
    exp(DataContML())
  } else {DataContML()
  }
  
  
})


# set character
observeEvent(transform(),{
  
  AncContMl$setCharacterContMl <- setNames(transform(), row.names(CharInput()))
  
  if(input$runAncML >0 & !is.null(AncContMl$Models)){
    showModal(   modalDialog(
      title = 'You have change the transformation type',
      'Please, re-run the analysis',
      easyClose = TRUE,
      footer = modalButton("Dismiss")))
    
    AncContMl$objetContinuousML <- NULL
    AncContMl$Models <- NULL
    AncContMl$setCharacterContMl <-NULL
    
    treeContML <- NULL
    
    DataContML <- NULL
    
    reset('ResetConCharML')
  }
  
})

#Temporal object to print in info panel
# info: output per every model run
#
observeEvent(input$transform1, {
  if(!is.null(transform())){
    AncContMl$objetContinuousML <- transform()
  }
})





#Plot initial tree if models do not have been estimated, otherwise plot model output
#

heightContMl <- reactive(input$PlotHeightContMl[1])
widthContMl <- reactive(input$PlotWidthContMl[1])

output$PhyloPlot2 <- renderPlot(height = heightContMl  , width = widthContMl, {

  req(treeContML())
  
  if(input$runAncML >0 & !is.null(AncContMl$Models)){
  if (input$phenogramML == 1) {
    if (input$mapModelMl == 'BM') {
      phenogram(tree = treeContML(), #as.phylo method to plot in black color
                x =  c(fixvar(),AncContMl$Models$BM$ace),
                spread.cost = c(1,0), 
                fsize = input$tipSizeContMl[1],
                lwd=0.8,colors ='grey40' )
      
    } else if (input$mapModelMl == 'EB') {
      phenogram(tree = treeContML(), #as.phylo method to plot in black color
                x =  c(fixvar(),AncContMl$Models$EB$ace),
                spread.cost = c(1,0), 
                fsize = input$tipSizeContMl[1],
                lwd=0.8,colors ='grey40')
    } else if (input$mapModelMl == 'OU') {
      phenogram(tree = treeContML(), #as.phylo method to plot in black color
                x =  c(fixvar(),AncContMl$Models$OU$ace),
                spread.cost = c(1,0), 
                fsize = input$tipSizeContMl[1],
                lwd=0.8,colors ='grey40')
    }
  }else{
  
  if (input$mapModelMl == 'BM') {
    plotBM <- contMap(tree = treeContML(),
                      x = fixvar(),
                      method = 'user',
                      anc.states = c(fixvar(),AncContMl$Models$BM$ace),
                      show.tip.label = T,
                      plot = FALSE
                      )
    n <- length(plotBM$cols)
    plotBM$cols[1:n] <- colorRampPalette(c("#02b2ce","#ffd004",  "#e52920"), bias=1.5) (n)
    plot(plotBM, outline = F,lwd =2.5,fsize = input$tipSizeContMl[1])
  } else if (input$mapModelMl == 'EB') {
    plotEB <- contMap(tree = treeContML(), 
                      x = fixvar(),
                      method = 'user',
                      anc.states = c(fixvar(),AncContMl$Models$EB$ace),
                      show.tip.label = T,
                      plot = FALSE)
    n <- length(plotEB$cols)
    plotEB$cols[1:n] <- colorRampPalette(c("#02b2ce","#ffd004",  "#e52920"), bias=1.5) (n)
    plot(plotEB, outline = F,lwd =2.5,fsize = input$tipSizeContMl[1])
  } else if (input$mapModelMl == 'OU') {
    plotOU <- contMap(tree = treeContML(), 
                      x = fixvar(),
                      method = 'user',
                      anc.states = c(fixvar(),AncContMl$Models$OU$ace),
                      show.tip.label = T,
                      plot = FALSE)
    n <- length(plotOU$cols)
    plotOU$cols[1:n] <- colorRampPalette(c("#02b2ce","#ffd004",  "#e52920"), bias=1.5) (n)
    plot(plotOU, outline = F,lwd =2.5,fsize = input$tipSizeContMl[1])
  }
    } 
  }else  {
      plot.phylo(x = treeContML(),
                 show.tip.label = T,
                 cex = input$tipSizeContMl[1],
                 use.edge.length = T,edge.width = 0.8,edge.color = 'grey40')
      }
  })


#Plot histogram or frequency distribution
#
output$histo1 <- renderPlot( {
 
  # colors1 <-col2rgb(paletteer::paletteer_c("grDevices::Purple-Yellow", 1))
  # color2 <- rgb(red = colors1[1,]/255,green =colors1[2,]/255,blue = colors1[3,]/255,alpha = 0.5)
  
  color1 <- colorRampPalette(c("#128cb0","#ffd004",  "#e52920"), bias=1.5) (10)
  
  if (input$AncPIC == 1) { #if PIC is required just for visualized, it is not taken into account for analysing
   
    h <- hist(x =  pic(x = transform(), phy = treeContML()),plot=F)

    par(adj=0.5,
        cex.axis=0.9,
        cex.lab= 1,
        cex.main= 1.2,
        family="sans")
    
    xlim <- range(h$mids)
    ylim <- range(h$counts)
    
    
    plot(NA, xlim = xlim , ylim =c(0, max(ylim)*1.1),bty = "n",  las = 1,axes = F,
         main= 'Histogram: Phylogenetic Independent Contrasts', sub = NULL, 
         xlab= input$dataVar, ylab= ' N Tips' )
    
    grid( lty = 2, lwd = 0.3)
    axis(side = 1, lwd=0.5, lwd.ticks = .5,mgp = c(3, 1, 0))
    axis(side = 2, lwd=0.5,lwd.ticks = .5,las = 2, mgp = c(3, 1, 0.5))
    hist(x =  pic(x = transform(), phy = treeContML()), add = TRUE, 
         col=make.transparent(color1[1], 0.5), border= make.transparent(color1[1], 0.8) )
    
    # h <- hist(x = pic(x = transform(), phy = treeContML()),plot=F)
    # 
    # par(
    #     cex.axis=0.7,
    #     cex.lab= 0.9,
    #     cex.main= 1,
    #     family="sans"
    # )
    # plot(h$mids, h$counts, yaxs = "i",
    #      type = 'n', bty = 'n',axes = F,main= 'Histogram: Phylogenetic Independent Contrasts', sub = NULL, xlab= input$dataVar, ylab= ' N Tips' )
    # grid(10,10, lty = 2, lwd = 0.3)
    # axis(side = 1,xlim = range(h$mids)*1.1, lwd=0, lwd.ticks = .5)
    # axis(side = 2,ylim = c(0, max(h$counts)), lwd=0,lwd.ticks = .5)
    # hist(x = pic(x = transform(), phy = treeContML()), add = TRUE, border= F,
    #      col=color2)
    # box(lwd= 0.5, col= 'grey')
    # 
  
  } else {
    
    h <- hist(x =  transform(),plot=F)
    
    par(adj=0.5,
        cex.axis=0.9,
        cex.lab= 1,
        cex.main= 1.2,
        family="sans")
    
    xlim <- range(h$mids)
    ylim <- range(h$counts)
    
    
    plot(NA, xlim = xlim , ylim =c(0, max(ylim)*1.1),bty = "n",  las = 1,axes = F,
         main= 'Histogram: Phylogenetic Independent Contrasts', sub = NULL, 
         xlab= input$dataVar, ylab= ' N Tips' )
    
    grid( lty = 2, lwd = 0.3)
    axis(side = 1, lwd=0.5, lwd.ticks = .5,mgp = c(3, 1, 0))
    axis(side = 2, lwd=0.5,lwd.ticks = .5,las = 2, mgp = c(3, 1, 0.5))
    
    hist(x = transform(), add = TRUE, 
         col=make.transparent(color1[1], 0.5), border= make.transparent(color1[1], 0.8) )
    # h <- hist(x = transform(),plot=F)
    # par(
    #     cex.axis=0.7,
    #     cex.lab= 0.9,
    #     cex.main= 1,
    #     family="sans"
    # )
    # plot(h$mids, h$counts, yaxs = "i",
    #      type = 'n', bty = 'n',axes = F, main='Histogram: raw data', sub = NULL, xlab= input$dataVar, ylab= ' N Tips')
    # grid(10,10, lty = 2, lwd = 0.3)
    # axis(side = 1,xlim = range(h$mids)*1.1, lwd=0, lwd.ticks = .5)
    # axis(side = 2,ylim = c(0, max(h$counts)), lwd=0,lwd.ticks = .5)
    # hist(x = transform(), add = TRUE, border= F,
    #      col= color2)
    # box(lwd= 0.5, col= 'grey')
  }
  
})


# Plot QQ plot for fitting to a normal distribution
# 
output$QQ1 <- renderPlot( {
  
  color1 <- colorRampPalette(c("#128cb0","#ffd004",  "#e52920"), bias=1.5)(10)

    if (input$AncPIC == 1) {
      q <-qqnorm(y = pic(x = transform(), phy = treeContML()),plot=F)
      par(adj=0.5,
          cex.axis=0.9,
          cex.lab= 1,
          cex.main= 1.2,
          family="sans")
      xlim <- range(q$x)
      ylim <- range(q$y)
      plot(q$x, q$y, xlim = xlim, ylim = ylim, cex=1.2,
           type = 'p',axes = F,main = 'Normal Q-Q Plot: PIC', sub = NULL, xlab = 'Theoretical Quantiles', ylab = 'PIC Quantiles',
           col=make.transparent(color1[1], 0.8),pch=21,bg=make.transparent(color1[1], 0.2))
      abline(lm(q$y ~ q$x),lwd= 0.8, col=make.transparent(color1[10], 0.8))
      grid( lty = 2, lwd = 0.3)
      axis(side = 1, lwd=0.5, lwd.ticks = .5,mgp = c(3, 1, 0))
      axis(side = 2, lwd=0.5,lwd.ticks = .5,las = 2, mgp = c(3, 1, 0.5))
      
  } else {
    q <-qqnorm(y = transform(),plot=F)
    par(adj=0.5,
        cex.axis=0.9,
        cex.lab= 1,
        cex.main= 1.2,
        family="sans")
    xlim <- range(q$x)
    ylim <- range(q$y)
    plot(q$x, q$y, xlim = xlim, ylim = ylim, cex=1.2,
         type = 'p',axes = F,main = 'Normal Q-Q Plot: PIC', sub = NULL, xlab = 'Theoretical Quantiles', ylab = 'Tip Quantiles',
         col=make.transparent(color1[1], 0.8),pch=21,bg=make.transparent(color1[1], 0.2))
    abline(lm(q$y ~ q$x),lwd= 0.8, col=make.transparent(color1[10], 0.8))
    grid( lty = 2, lwd = 0.3)
    axis(side = 1, lwd=0.5, lwd.ticks = .5,mgp = c(3, 1, 0))
    axis(side = 2, lwd=0.5,lwd.ticks = .5,las = 2, mgp = c(3, 1, 0.5))
    
    # q <-qqnorm(y = transform(),plot=F)
    # par(adj=0.5,
    #     cex.axis=0.7,
    #     cex.lab= 0.9,
    #     cex.main= 1,
    #     family="sans"
    # )
    # 
    # plot(q$x, q$y, yaxs = "i", 
    #      type = 'p',axes = F,main = 'Normal Q-Q Plot: PIC', sub = NULL, xlab = 'Theoretical Quantiles', ylab = 'Tip Quantiles',
    #      col=color3,pch=21,bg=color2)
    # grid( lty = 2, lwd = 0.3)
    # axis(side = 1,xlim = range(q$x)*1.1,  lwd=0, lwd.ticks = .5)
    # axis(side = 2,ylim = c(0, max(q$y)), lwd=0,lwd.ticks = .5)
    # box(lwd= 0.5, col= 'grey')
  }
  
})

#Fit models: Run Analysis
#
observeEvent(input$runAncML, {
  

  anc.ML2 <- function(tree, x, maxit = 2000, model = c("BM", "OU", "EB"), ...){
    tryCatch(phytools::anc.ML(tree, x, maxit = maxit, model = model, ...), 
             error = function(e) {
               message('No data in chosen Analysis.', e)
               showModal(
                 modalDialog(
                   title = paste0('Error in',model, ' model'),
                   e$message,
                   easyClose = TRUE,
                   footer = modalButton("Dismiss"))
               )
               #showNotification(ui = paste0('Error in',model, ' model:',e$message, e),duration = 7,type = 'error',closeButton = T)
               return(NULL)
             })
  }

  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
               
                 incProgress(1/4)
            
  
  
  if ( "BM" %in% input$ModelContinuous ) {
    AncContMl$Models$BM <- anc.ML2(tree = treeContML(), 
                          x = fixvar(), 
                          maxit = input$maxitBM[1], 
                          model = 'BM')
  }
                 incProgress(2/4)
  
  if ("EB" %in% input$ModelContinuous) { 
    AncContMl$Models$EB <- anc.ML2(tree = treeContML(),
                          x =  fixvar(), 
                          maxit = input$maxitEB[1], 
                          model = 'EB')
  }
                 incProgress(3/4)
  
  if ("OU" %in% input$ModelContinuous) {
    AncContMl$Models$OU <- anc.ML2(tree = treeContML(),
                          x = fixvar(),
                          maxit = input$maxitOU[1],
                          model = 'OU')
  }
                 
                 incProgress(3.5/4)
                 
                 
               })
  
})






# update  Selection input to plot an specific model that had been run
# 
observeEvent(input$runAncML, {
  updateSelectInput(session = session, inputId = "mapModelMl", choices = names(AncContMl$Models))
  
  
})

#
fixvar <- eventReactive(input$runAncML,{
  AncContMl$setCharacterContMl
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

observeEvent(input$ResetConMl,{

  reset('mapModelMl')
  AncContMl$objetContinuousML <- NULL
  AncContMl$Models <- NULL
  AncContMl$setCharacterContMl <-NULL
  
  treeContML <- NULL
  
  DataContML <- NULL

  reset('ResetConCharML')
})