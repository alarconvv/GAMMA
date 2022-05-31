##############################################################################
# Discrete Character : Maximum Likelihood
##############################################################################


# Info panel
output$infoPanelDiscreteML <- renderPrint({
  if (!is.null(AncDiscMl$objectDiscreteML)){
    print(AncDiscMl$objectDiscreteML)
  }
})

# Vector to store models and outputs
# 

AncDiscMl <- reactiveValues()

AncDiscMl$multiStatesModels <- list()
AncDiscMl$outcorHMM <- list()
AncDiscMl$counter <- 0
AncDiscMl$AICdf <- NULL
AncDiscMl$AICw <- NULL
AncDiscMl$w.perNode <- list()
AncDiscMl$modelAverage <- NULL
AncDiscMl$objectDiscreteML  <- NULL
AncDiscMl$setCharacterML <- NULL



#Tree

treeDisML <- eventReactive(c( treeInput()),{
  validate(
    need(try(treeInput()), "Please select a tree")
  )
  treeInput()
}
)


#Tree

DataDisML  <- eventReactive(c(SelectedVarDisc()),{
  validate(
    need(try(SelectedVarDisc()), "Please select a data set")
  )
  SelectedVarDisc()
}
)




# n states
#
nStates <- eventReactive(c(DataDisML()),{length(levels(DataDisML()))})


# set character
observeEvent(DataDisML(),{
  AncDiscMl$setCharacterML <- setNames(DataDisML(),row.names(CharInput()))
})




observeEvent(DataDisML(),{
  


  if ( nStates()[1]== 2){
    # update model list
    updateSelectInput(session, "ModelsDisML",
                      choices=c('ER'='ER', 'ARD'='ARD', 'Ireversible01'='Ireversible01','Ireversible10'='Ireversible10'))
  }else if ( nStates()[1] > 2) {
    # update model list for 3 states
    updateSelectInput(session, "ModelsDisML",
                      choices=c('ER'='ER', 'ARD'='ARD', 'SYM'='SYM'))
    
    
    # display  add buttonaction
    output$addModel3States <- renderUI({
      actionButton('AddModelDisML','Add model')
    })
    
  }
})  



observeEvent(!is.null(input$ModelsDisML),{
  
  #Create a matrix0
  AncDiscMl$matrix0 <- matrix(NA, nStates()[1], nStates()[1],dimnames=list(levels(DataDisML()),levels(DataDisML())))
  
  
  # Create ER model
  if (  'ER'  %in% input$ModelsDisML){
    AncDiscMl$multiStatesModels$ER <- AncDiscMl$matrix0
    
    row.names(AncDiscMl$multiStatesModels$ER) <- levels(DataDisML())
    colnames(AncDiscMl$multiStatesModels$ER) <- levels(DataDisML())
    
    AncDiscMl$multiStatesModels$ER[lower.tri(AncDiscMl$multiStatesModels$ER)] <- 1
    AncDiscMl$multiStatesModels$ER[upper.tri(AncDiscMl$multiStatesModels$ER)] <- 1
  }
  
  # Create ARD model
  if ('ARD' %in% input$ModelsDisML){
    AncDiscMl$multiStatesModels$ARD <- AncDiscMl$matrix0
    
    row.names(AncDiscMl$multiStatesModels$ARD) <- levels(DataDisML())
    colnames(AncDiscMl$multiStatesModels$ARD) <- levels(DataDisML())
    
    AncDiscMl$multiStatesModels$ARD[lower.tri(AncDiscMl$multiStatesModels$ARD)] <- 2
    AncDiscMl$multiStatesModels$ARD[upper.tri(AncDiscMl$multiStatesModels$ARD)] <- 1
  }
  
  # Create SYM model
  if ('SYM' %in% input$ModelsDisML){
    AncDiscMl$multiStatesModels$SYM <- AncDiscMl$matrix0
    
    row.names(AncDiscMl$multiStatesModels$SYM) <- levels(DataDisML())
    colnames(AncDiscMl$multiStatesModels$SYM) <- levels(DataDisML())
    
    AncDiscMl$multiStatesModels$SYM[lower.tri(AncDiscMl$multiStatesModels$SYM)] <- 1:length(AncDiscMl$multiStatesModels$SYM[lower.tri(AncDiscMl$multiStatesModels$SYM)])
    AncDiscMl$multiStatesModels$SYM[upper.tri(AncDiscMl$multiStatesModels$SYM)] <- 1:length(AncDiscMl$multiStatesModels$SYM[lower.tri(AncDiscMl$multiStatesModels$SYM)])
  }
  
  # Create  Ireversible01
  if ('Ireversible01' %in% input$ModelsDisML){
    AncDiscMl$multiStatesModels$Ireversible01 <- AncDiscMl$matrix0
    
    row.names(AncDiscMl$multiStatesModels$Ireversible01) <- levels(DataDisML())
    colnames(AncDiscMl$multiStatesModels$Ireversible01) <- levels(DataDisML())
    
    AncDiscMl$multiStatesModels$Ireversible01[lower.tri(AncDiscMl$multiStatesModels$Ireversible01)] <- 1
    AncDiscMl$multiStatesModels$Ireversible01[upper.tri(AncDiscMl$multiStatesModels$Ireversible01)] <- 0
  }
  
  # Create Ireversible10
  if ('Ireversible10' %in% input$ModelsDisML){
    AncDiscMl$multiStatesModels$Ireversible10 <- AncDiscMl$matrix0
    
    row.names(AncDiscMl$multiStatesModels$Ireversible10) <- levels(DataDisML())
    colnames(AncDiscMl$multiStatesModels$Ireversible10) <- levels(DataDisML())
    
    AncDiscMl$multiStatesModels$Ireversible10[lower.tri(AncDiscMl$multiStatesModels$Ireversible10)] <- 0
    AncDiscMl$multiStatesModels$Ireversible10[upper.tri(AncDiscMl$multiStatesModels$Ireversible10)] <- 1
  }
})



# if add model button is clicked
observeEvent(input$AddModelDisML > 0 ,{
  

  # add matrix for a new model
  output$w1 <- renderRHandsontable({
    AncDiscMl$matrix0[lower.tri(AncDiscMl$matrix0)] <- as.integer(1)
    AncDiscMl$matrix0[upper.tri(AncDiscMl$matrix0)] <- as.integer(1)
    rhandsontable(AncDiscMl$matrix0,readOnly = F)
  })
  
  
})





#Submitting models

observeEvent(input$SubmAddModel > 0,{
  
  # star counter
  AncDiscMl$counter[1] <- AncDiscMl$counter[1] + 1

  AncDiscMl$multiStatesModels$x <- hot_to_r(input$w1)
  row.names(AncDiscMl$multiStatesModels$x) <- levels(DataDisML())
  colnames(AncDiscMl$multiStatesModels$x) <- levels(DataDisML())
  
  names(AncDiscMl$multiStatesModels)[length(AncDiscMl$multiStatesModels)] <- paste('UserModel',as.character(AncDiscMl$counter[1] - 1),sep = '')
  
  updateSelectInput(session, "ModelsDisML",
                    choices = names(AncDiscMl$multiStatesModels),selected = names(AncDiscMl$multiStatesModels))

  
 })



# info panel
observeEvent(!is.null(input$ModelsDisML),{
  AncDiscMl$objectDiscreteML <- AncDiscMl$multiStatesModels
})



# Run Analysis for 2 states
observeEvent(input$RunAnalyDisML ,{
  
  SppData<-data.frame(Genus_sp=names(AncDiscMl$setCharacterML),x=AncDiscMl$setCharacterML)
  
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 
                 incProgress(1/2)
  
  if ( nStates()[1]== 2){
    if ('ER' %in% input$ModelsDisML ){
      AncDiscMl$outcorHMM$ER <- corHMM(treeDisML(),SppData,node.states=input$typeDisML[1],
                                           rate.cat=1,rate.mat=matrix(c(NA,1,1,NA),2,2),root.p=c(0.5,0.5))}
    if ('ARD' %in% input$ModelsDisML){
      AncDiscMl$outcorHMM$ARD <- corHMM(treeDisML(),SppData,node.states=input$typeDisML[1],
                                            rate.cat=1,rate.mat=matrix(c(NA,1,2,NA),2,2),root.p=c(0.5,0.5))}
    if ('Ireversible01' %in% input$ModelsDisML){
      AncDiscMl$outcorHMM$Ireversible01 <- corHMM(treeDisML(),SppData,node.states=input$typeDisML[1],
                                                      rate.cat=1,rate.mat=matrix(c(NA,0,1,NA),2,2),root.p=c(0.5,0.5))}
    if ('Ireversible10' %in% input$ModelsDisML){
      AncDiscMl$outcorHMM$Ireversible10<- corHMM(treeDisML(),SppData,node.states=input$typeDisML[1],
                                                     rate.cat=1,rate.mat=matrix(c(NA,1,0,NA),2,2),root.p=c(0.5,0.5))}
  }
  
  if ( nStates()[1] > 2){
    
    num <- which( names(AncDiscMl$multiStatesModels) %in% input$ModelsDisML)
    
    for (i in num){
      
      AncDiscMl$outcorHMM$x <- corHMM(treeDisML(),SppData,node.states=input$typeDisML[1],
                                          rate.cat=1,rate.mat=AncDiscMl$multiStatesModels[[i]])
      names(AncDiscMl$outcorHMM)[length(AncDiscMl$outcorHMM)] <- names(AncDiscMl$multiStatesModels)[i]
      
    }
    
    
  }
                 incProgress(2/2)
                 
                 
               })
  
  
})

## info panel
observeEvent(!is.null(input$RunAnalyDisML ),{
  AncDiscMl$objectDiscreteML <- AncDiscMl$outcorHMM
})




## info panel
observeEvent(input$DisMLModAIC,{
  
  
  logLik.corhmm <- function(object,...){
    lik<-object$loglik
    attr(lik,"df")<-max(object$index.mat,na.rm=TRUE)
    lik
  }
  
  AICc<-function(object,...){
    aicc<-object$AICc
  }
  
  
  
  foo<-function(object){
    setNames(c(attributes(logLik.corhmm(object))$df,logLik.corhmm(object),AIC(object),AICc(object)),
             c("df","logLik","AIC","AICc"))
  }
  
  AncDiscMl$AICdf<-as.data.frame(t(sapply(AncDiscMl$outcorHMM,foo)))
  
  
})


## info panel
observeEvent(!is.null(input$DisMLModAIC),{
  AncDiscMl$objectDiscreteML <- AncDiscMl$AICdf
})




# update  setting plot
observeEvent(input$ModAverDisML,{
  updateSelectInput(session, "SetModAverDisML",choices=names(AncDiscMl$outcorHMM))
  
  AncDiscMl$objectDiscreteML <- ' Please, choose models to average'
  
})



# calculate AIC.w and Run model average
observeEvent(input$RunModAverDisML,{
  
  toAverage <- which(names(AncDiscMl$outcorHMM) %in% input$SetModAverDisML)
  
  AncDiscMl$AICw <- as.matrix(setNames(aic.w(AncDiscMl$AICdf$AIC[toAverage]),names(AncDiscMl$outcorHMM)[toAverage]))
  
  
  
  # Geting Data from corrHMM object
  for (i in 1:length(toAverage)){
    
    if (input$typeDisML == 'joint'){
      AncDiscMl$w.perNode$x <- to.matrix(AncDiscMl$outcorHMM[[toAverage[i]]]$states,1:length(levels(DataDisML()))) * (AncDiscMl$AICw[i,])
    }else{
      AncDiscMl$w.perNode$x <- AncDiscMl$outcorHMM[[toAverage[i]]]$states * (AncDiscMl$AICw[i,])
    }
    
    names(AncDiscMl$w.perNode)[length(AncDiscMl$w.perNode)] <- input$SetModAverDisML[i]
  }
  
  
  # make an array from the list to calculate de mean
  arr <- array( unlist(AncDiscMl$w.perNode) , c(nrow(AncDiscMl$w.perNode[[1]]),ncol(AncDiscMl$w.perNode[[1]]),length(AncDiscMl$w.perNode)) )
  
  # Calcualte the mean per character
  AncDiscMl$modelAverage <- apply( arr, 1:2, sum )
  
  colnames(AncDiscMl$modelAverage) <- levels(DataDisML())
  
})



## info panel
observeEvent(!is.null(input$RunModAverDisML),{
  AncDiscMl$objectDiscreteML <- AncDiscMl$modelAverage
})




# initial and output tree / phenogram if it is chosen

heightDisML <- reactive(input$PlotHeightDisML[1])
widthDisML <- reactive(input$PlotWidthDisML[1])
#plot phylogeny: Disc Char
output$PhyloPlot8 <- renderPlot(height = heightDisML  , width = widthDisML,{
  
  DisColPal <- paletteer::paletteer_c("grDevices::Purple-Yellow", length(levels(DataDisML())))
  
  DisCols <- setNames(DisColPal,levels(DataDisML()))
  
  if (input$RunAnalyDisML == 1 ){
    if (input$plotModelDisML == 'ModelAverage'){
      legend('topright',legend = levels(DataDisML()),pch = 22,pt.cex=1.5, pt.bg = DisCols, bty='n',cex = 0.8)
      
      pieDiscML <- AncDiscMl$modelAverage
      
      if (input$bestState == 1){
        nn <- apply(pieDiscML, 1,function(x) which(x == max(x)))
        pieDiscML[,1:ncol(pieDiscML)] <- 0 
        
        for (i in 1:length(nn)) {
          pieDiscML[i,nn[[i]]] <- 1
        }
      }
      
      plotTree.datamatrix(treeDisML(),as.data.frame(AncDiscMl$setCharacterML),colors=list(DisCols),header=FALSE,fsize=input$tipSizeDisML[1],lwd = 0.8)
      
      legend('topright',legend = levels(DataDisML()),pch = 22,pt.cex=1.5, pt.bg = DisCols, bty='n',cex = 0.8)
      
      nodelabels(pie=pieDiscML,piecol=DisCols,cex=0.5)
      
      
      
    }else{
      
      
      Dimod <-which(names(AncDiscMl$outcorHMM) == input$plotModelDisML)
      
      
      if (input$typeDisML == 'joint'){
        pieDiscML <- to.matrix(AncDiscMl$outcorHMM[[Dimod[1]]]$states,1:length(levels(DataDisML())))
      }else{
        pieDiscML <- AncDiscMl$outcorHMM[[Dimod[1]]]$states
      }
      
      if (input$bestState == 1){
        nn <- apply(pieDiscML, 1,function(x) which(x == max(x)))
        pieDiscML[,1:ncol(pieDiscML)] <- 0 
        
        for (i in 1:length(nn)) {
          pieDiscML[i,nn[[i]]] <- 1
        }
      }
      
      if (length(Dimod) > 0){
        plotTree.datamatrix(treeDisML(),as.data.frame(AncDiscMl$setCharacterML),colors=list(DisCols),header=FALSE,fsize=input$tipSizeDisML[1],lwd = 0.8)
        
        legend('topright',legend = levels(DataDisML()),pch = 22,pt.cex=1.5, pt.bg = DisCols, bty='n',cex = 0.8)
        
        nodelabels(pie=pieDiscML,piecol=DisCols,cex=0.5)
      }
      
    }
    
  }else {

    
    plotTree.datamatrix(treeDisML(),as.data.frame(AncDiscMl$setCharacterML),colors=list(DisCols),header=FALSE,fsize=input$tipSizeDisML[1],lwd = 0.8)
    
    legend('topright',legend = levels(DataDisML()),pch = 22,pt.cex=1.5, pt.bg = DisCols, bty='n',cex = 0.8)
    
  }
  
})






# Plot models
observeEvent(input$ModelsDisML,{
  output$PhyloPlot9 <- renderPlot({
    
    as.Qmatrix.matrix <- function(x, level,...){
      colnames(x) <- levels(level)
      row.names(x) <- levels(level)
      diag(x) <- -rowSums(x,na.rm = T)
      class(x) <- 'Qmatrix'
      x
    }
    
    as.Qmatrix.corhmm<-function(x,level,...){
      
      Q<-x$solution
      Q[which(is.na(Q))] <- 0
      diag(Q)<--rowSums(Q,na.rm=TRUE)
      #diag(Q)<--apply(Q,1,sum)
      class(Q)<-"Qmatrix"
      colnames(Q) <- levels(level)
      row.names(Q) <- levels(level)
      Q
    }
    
    if (input$RunAnalyDisML == 1 ){
      
      if (input$plotModelDisML == 'ModelAverage'){
        
        par(mfrow = c(1,1))
        part <- ceiling(length(input$SetModAverDisML)/2)
        par(mfrow = c(part,2),mar=c(1,1,1,1))
        
        Dimod <-which(names(AncDiscMl$outcorHMM) %in% input$SetModAverDisML)
        
        for (i in 1:length(input$SetModAverDisML)){
          
          plot(as.Qmatrix.corhmm(AncDiscMl$outcorHMM[[Dimod[i]]],DataDisML()),main= input$SetModAverDisML[i],show.zeros=FALSE)
          
        }
        
      }else{
        
        par(mfrow = c(1,1),mar=c(1,1,1,1))
        Dimod <-which(names(AncDiscMl$outcorHMM) == input$plotModelDisML)
        
        if (length(Dimod) > 0){
          plot(as.Qmatrix.corhmm(AncDiscMl$outcorHMM[[Dimod]],DataDisML()),main= input$plotModelDisML[1],show.zeros=FALSE)
        }
      }
      
    }else{
      
      if (length(AncDiscMl$multiStatesModels) > 1){
        par(mfrow = c(1,1))
        part <- ceiling(length(AncDiscMl$multiStatesModels)/2)
        par(mfrow = c(part,2),mar=c(1,1,1,1))
        
        for (i in 1:length(AncDiscMl$multiStatesModels)){
          plot(as.Qmatrix.matrix(AncDiscMl$multiStatesModels[[i]],DataDisML()),main= names(AncDiscMl$multiStatesModels)[i],show.zeros=FALSE)
        }
      }else{
        par(mfrow = c(1,1),mar=c(1,1,1,1))
        
        for (i in 1:length(AncDiscMl$multiStatesModels)){
          
          plot(as.Qmatrix.matrix(AncDiscMl$multiStatesModels[[i]],DataDisML()),main= names(AncDiscMl$multiStatesModels)[i],show.zeros=FALSE)
        }
      }
    }
    
  })
})


#Update models to plot

observeEvent(input$RunAnalyDisML,{
  
  updateSelectInput(session, "plotModelDisML",choices=names(AncDiscMl$outcorHMM))
})


observeEvent(input$RunModAverDisML,{
  updateSelectInput(session, "plotModelDisML",
                    choices=c(names(AncDiscMl$outcorHMM),'ModelAverage'),selected = 'ModelAverage')
  
})

# Downloadable TXT/RDS of selected dataset ----




output$downloadDisML <- downloadHandler(
  filename = function(){
    if (input$exportDisMLanc== 'MLancTXT' & input$disCharOutputs == "ModelAverage"){
      c('Out_AncDis_ModAverage_ML.txt')
    }else if (input$exportDisMLanc== "MLancRDS" & input$disCharOutputs == "ModelAverage"){
      c('Out_AncDis_ModAverage_ML.RDS')
    }else if (input$exportDisMLanc== 'MLancTXT' & input$disCharOutputs == "AICmatrix"){
      c('Out_AncDis_AIC_ML.txt')
    }else if (input$exportDisMLanc== "MLancRDS" & input$disCharOutputs == "AICmatrix"){
      c('Out_AncDis_AIC_ML.RDS')
    }else if (input$exportDisMLanc== 'MLancTXT' & input$disCharOutputs == "FittedModels"){
      c('Out_AncDis_Models_ML.txt')
    }else if (input$exportDisMLanc== "MLancRDS" & input$disCharOutputs == "FittedModels"){
      c('Out_AncDis_Models_ML.RDS')
    }
  },
  
  content = function(file){
    if (input$exportDisMLanc== 'MLancTXT' & input$disCharOutputs == "ModelAverage"){
      utils::capture.output(AncDiscMl$modelAverage,file = file)
    }else if (input$exportDisMLanc== "MLancRDS" & input$disCharOutputs == "ModelAverage"){
      saveRDS(object = AncDiscMl$modelAverage,file = file)
    }else if (input$exportDisMLanc== 'MLancTXT' & input$disCharOutputs == "AICmatrix"){
      utils::capture.output(AncDiscMl$AICdf,file = file)
    }else if (input$exportDisMLanc== 'MLancRDS' & input$disCharOutputs == "AICmatrix"){
      saveRDS(object = AncDiscMl$AICdf,file = file)
    }else if (input$exportDisMLanc== 'MLancTXT' & input$disCharOutputs == "FittedModels"){
      utils::capture.output(AncDiscMl$outcorHMM,file = file)
    }else if (input$exportDisMLanc== 'MLancRDS' & input$disCharOutputs == "FittedModels"){
      saveRDS(object = AncDiscMl$outcorHMM,file = file)
    }
  }
)
