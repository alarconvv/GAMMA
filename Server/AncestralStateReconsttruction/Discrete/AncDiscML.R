
# Info panel
output$infoPanelDiscreteML <- renderPrint({
  if (!is.null(v$objectDiscreteML)){
    print(v$objectDiscreteML)
  }
})

# Vector to store models and outputs
# 

ModelsDiscret <- reactiveValues()

ModelsDiscret$multiStatesModels <- list()
ModelsDiscret$outcorHMM <- list()
ModelsDiscret$counter <- 0
ModelsDiscret$AICdf <- NULL
ModelsDiscret$AICw <- NULL
ModelsDiscret$w.perNode <- list()
ModelsDiscret$modelAverage <- NULL


nStates <- reactive(length(levels(SelectedVarDisc())))



# n states
observeEvent(input$typeChar == 'Discrete',{
  
  if (nStates()[1]== 2){
    # update model list
    updateSelectInput(session, "ModelsDisML",
                      choices=c('ER'='ER', 'ARD'='ARD', 'Ireversible01'='Ireversible01','Ireversible10'='Ireversible10'))
  }else {
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
  ModelsDiscret$matrix0 <- matrix(NA,nStates()[1],nStates()[1],dimnames=list(levels(SelectedVarDisc()),levels(SelectedVarDisc())))
  
  
  # Create ER model
  if (  'ER'  %in% input$ModelsDisML){
    ModelsDiscret$multiStatesModels$ER <- ModelsDiscret$matrix0
    
    row.names(ModelsDiscret$multiStatesModels$ER) <- levels(SelectedVarDisc())
    colnames(ModelsDiscret$multiStatesModels$ER) <- levels(SelectedVarDisc())
    
    ModelsDiscret$multiStatesModels$ER[lower.tri(ModelsDiscret$multiStatesModels$ER)] <- 1
    ModelsDiscret$multiStatesModels$ER[upper.tri(ModelsDiscret$multiStatesModels$ER)] <- 1
  }
  
  # Create ARD model
  if ('ARD' %in% input$ModelsDisML){
    ModelsDiscret$multiStatesModels$ARD <- ModelsDiscret$matrix0
    
    row.names(ModelsDiscret$multiStatesModels$ARD) <- levels(SelectedVarDisc())
    colnames(ModelsDiscret$multiStatesModels$ARD) <- levels(SelectedVarDisc())
    
    ModelsDiscret$multiStatesModels$ARD[lower.tri(ModelsDiscret$multiStatesModels$ARD)] <- 2
    ModelsDiscret$multiStatesModels$ARD[upper.tri(ModelsDiscret$multiStatesModels$ARD)] <- 1
  }
  
  # Create SYM model
  if ('SYM' %in% input$ModelsDisML){
    ModelsDiscret$multiStatesModels$SYM <- ModelsDiscret$matrix0
    
    row.names(ModelsDiscret$multiStatesModels$SYM) <- levels(SelectedVarDisc())
    colnames(ModelsDiscret$multiStatesModels$SYM) <- levels(SelectedVarDisc())
    
    ModelsDiscret$multiStatesModels$SYM[lower.tri(ModelsDiscret$multiStatesModels$SYM)] <- 1:length(ModelsDiscret$multiStatesModels$SYM[lower.tri(ModelsDiscret$multiStatesModels$SYM)])
    ModelsDiscret$multiStatesModels$SYM[upper.tri(ModelsDiscret$multiStatesModels$SYM)] <- 1:length(ModelsDiscret$multiStatesModels$SYM[lower.tri(ModelsDiscret$multiStatesModels$SYM)])
  }
  
  # Create  Ireversible01
  if ('Ireversible01' %in% input$ModelsDisML){
    ModelsDiscret$multiStatesModels$Ireversible01 <- ModelsDiscret$matrix0
    
    row.names(ModelsDiscret$multiStatesModels$Ireversible01) <- levels(SelectedVarDisc())
    colnames(ModelsDiscret$multiStatesModels$Ireversible01) <- levels(SelectedVarDisc())
    
    ModelsDiscret$multiStatesModels$Ireversible01[lower.tri(ModelsDiscret$multiStatesModels$Ireversible01)] <- 1
    ModelsDiscret$multiStatesModels$Ireversible01[upper.tri(ModelsDiscret$multiStatesModels$Ireversible01)] <- 0
  }
  
  # Create Ireversible10
  if ('Ireversible10' %in% input$ModelsDisML){
    ModelsDiscret$multiStatesModels$Ireversible10 <- ModelsDiscret$matrix0
    
    row.names(ModelsDiscret$multiStatesModels$Ireversible10) <- levels(SelectedVarDisc())
    colnames(ModelsDiscret$multiStatesModels$Ireversible10) <- levels(SelectedVarDisc())
    
    ModelsDiscret$multiStatesModels$Ireversible10[lower.tri(ModelsDiscret$multiStatesModels$Ireversible10)] <- 0
    ModelsDiscret$multiStatesModels$Ireversible10[upper.tri(ModelsDiscret$multiStatesModels$Ireversible10)] <- 1
  }
})



# if add model button is clicked
observeEvent(input$AddModelDisML > 0 ,{
  

  # add matrix for a new model
  output$w1 <- renderRHandsontable({
    ModelsDiscret$matrix0[lower.tri(ModelsDiscret$matrix0)] <- as.integer(1)
    ModelsDiscret$matrix0[upper.tri(ModelsDiscret$matrix0)] <- as.integer(1)
    rhandsontable(ModelsDiscret$matrix0,readOnly = F)
  })
  
  
})





#Submitting models

observeEvent(input$SubmAddModel > 0,{
  
  # star counter
  ModelsDiscret$counter[1] <- ModelsDiscret$counter[1] + 1

  ModelsDiscret$multiStatesModels$x <- hot_to_r(input$w1)
  row.names(ModelsDiscret$multiStatesModels$x) <- levels(SelectedVarDisc())
  colnames(ModelsDiscret$multiStatesModels$x) <- levels(SelectedVarDisc())
  
  names(ModelsDiscret$multiStatesModels)[length(ModelsDiscret$multiStatesModels)] <- paste('UserModel',as.character(ModelsDiscret$counter[1] - 1),sep = '')
  
  updateSelectInput(session, "ModelsDisML",
                    choices = names(ModelsDiscret$multiStatesModels),selected = names(ModelsDiscret$multiStatesModels))

  
 })



# info panel
observeEvent(!is.null(input$ModelsDisML),{
  v$objectDiscreteML <- ModelsDiscret$multiStatesModels
})



# Run Analysis for 2 states
observeEvent(input$RunAnalyDisML ,{
  
  SppData<-data.frame(Genus_sp=names(setNames(SelectedVarDisc(),row.names(CharInput()))),x=setNames(SelectedVarDisc(),row.names(CharInput())))
  
  if (nStates()[1]== 2){
    if ('ER' %in% input$ModelsDisML ){
      ModelsDiscret$outcorHMM$ER <- corHMM(treeInput(),SppData,node.states=input$typeDisML[1],
                                           rate.cat=1,rate.mat=matrix(c(NA,1,1,NA),2,2),root.p=c(0.5,0.5))}
    if ('ARD' %in% input$ModelsDisML){
      ModelsDiscret$outcorHMM$ARD <- corHMM(treeInput(),SppData,node.states=input$typeDisML[1],
                                            rate.cat=1,rate.mat=matrix(c(NA,1,2,NA),2,2),root.p=c(0.5,0.5))}
    if ('Ireversible01' %in% input$ModelsDisML){
      ModelsDiscret$outcorHMM$Ireversible01 <- corHMM(treeInput(),SppData,node.states=input$typeDisML[1],
                                                      rate.cat=1,rate.mat=matrix(c(NA,0,1,NA),2,2),root.p=c(0.5,0.5))}
    if ('Ireversible10' %in% input$ModelsDisML){
      ModelsDiscret$outcorHMM$Ireversible10<- corHMM(treeInput(),SppData,node.states=input$typeDisML[1],
                                                     rate.cat=1,rate.mat=matrix(c(NA,1,0,NA),2,2),root.p=c(0.5,0.5))}
  }
  
  if (nStates()[1] > 2){
    
    num <- which( names(ModelsDiscret$multiStatesModels) %in% input$ModelsDisML)
    
    for (i in num){
      
      ModelsDiscret$outcorHMM$x <- corHMM(treeInput(),SppData,node.states=input$typeDisML[1],
                                          rate.cat=1,rate.mat=ModelsDiscret$multiStatesModels[[i]])
      names(ModelsDiscret$outcorHMM)[length(ModelsDiscret$outcorHMM)] <- names(ModelsDiscret$multiStatesModels)[i]
      
    }
    
    
  }
  
  
})

## info panel
observeEvent(!is.null(input$RunAnalyDisML ),{
  v$objectDiscreteML <- ModelsDiscret$outcorHMM
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
    setNames(c(attributes(logLik(object))$df,logLik(object),AIC(object),AICc(object)),
             c("df","logLik","AIC","AICc"))
  }
  
  ModelsDiscret$AICdf<-as.data.frame(t(sapply(ModelsDiscret$outcorHMM,foo)))
  
  
})


## info panel
observeEvent(!is.null(input$DisMLModAIC),{
  v$objectDiscreteML <- ModelsDiscret$AICdf
})




# update  setting plot
observeEvent(input$ModAverDisML,{
  updateSelectInput(session, "SetModAverDisML",choices=names(ModelsDiscret$outcorHMM))
  
  v$objectDiscreteML <- ' Please, choose models to average'
  
})



# calculate AIC.w and Run model average
observeEvent(input$RunModAverDisML,{
  
  toAverage <- which(names(ModelsDiscret$outcorHMM) %in% input$SetModAverDisML)
  
  ModelsDiscret$AICw <- as.matrix(setNames(aic.w(ModelsDiscret$AICdf$AIC[toAverage]),names(ModelsDiscret$outcorHMM)[toAverage]))
  
  
  
  # Geting Data from corrHMM object
  for (i in 1:length(toAverage)){
    
    if (input$typeDisML == 'joint'){
      ModelsDiscret$w.perNode$x <- to.matrix(ModelsDiscret$outcorHMM[[toAverage[i]]]$states,1:length(levels(SelectedVarDisc()))) * (ModelsDiscret$AICw[i,])
    }else{
      ModelsDiscret$w.perNode$x <- ModelsDiscret$outcorHMM[[toAverage[i]]]$states * (ModelsDiscret$AICw[i,])
    }
    
    names(ModelsDiscret$w.perNode)[length(ModelsDiscret$w.perNode)] <- input$SetModAverDisML[i]
  }
  
  
  # make an array fron the list to calculate de mean
  arr <- array( unlist(ModelsDiscret$w.perNode) , c(nrow(ModelsDiscret$w.perNode[[1]]),ncol(ModelsDiscret$w.perNode[[1]]),length(ModelsDiscret$w.perNode)) )
  
  # Calcualte the mean per character
  ModelsDiscret$modelAverage <- apply( arr, 1:2, sum )
  
  colnames(ModelsDiscret$modelAverage) <- levels(SelectedVarDisc())
  
})



## info panel
observeEvent(!is.null(input$RunModAverDisML),{
  v$objectDiscreteML <- ModelsDiscret$modelAverage
})


#plot phylogeny: Disc Char
output$PhyloPlot8 <- renderPlot({
  
  if (input$RunAnalyDisML == 1 ){
    if (input$plotModelDisML == 'ModelAverage'){
      DisColPal <- paletteer::paletteer_c("grDevices::Purple-Yellow", length(levels(SelectedVarDisc())))
      
      DisCols <- setNames(DisColPal,levels(SelectedVarDisc()))
      
      legend('topright',legend = levels(SelectedVarDisc()),pch = 22,pt.cex=1.5, pt.bg = DisCols, bty='n',cex = 0.8)
      
      pieDiscML <- ModelsDiscret$modelAverage
      
      if (input$bestState == 1){
        nn <- apply(pieDiscML, 1,function(x) which(x == max(x)))
        pieDiscML[,1:ncol(pieDiscML)] <- 0 
        
        for (i in 1:length(nn)) {
          pieDiscML[i,nn[[i]]] <- 1
        }
      }
      
      plotTree.datamatrix(treeInput(),as.data.frame(setNames(SelectedVarDisc(),row.names(CharInput()))),colors=list(DisCols),header=FALSE,fsize=0.45)
      
      legend('topright',legend = levels(SelectedVarDisc()),pch = 22,pt.cex=1.5, pt.bg = DisCols, bty='n',cex = 0.8)
      
      nodelabels(pie=pieDiscML,piecol=DisCols,cex=0.5)
      
      
      
    }else{
      
      DisColPal <- paletteer::paletteer_c("grDevices::Purple-Yellow", length(levels(SelectedVarDisc())))
      
      DisCols <- setNames(DisColPal,levels(SelectedVarDisc()))
      
      Dimod <-which(names(ModelsDiscret$outcorHMM) == input$plotModelDisML)
      
      
      if (input$typeDisML == 'joint'){
        pieDiscML <- to.matrix(ModelsDiscret$outcorHMM[[Dimod[1]]]$states,1:length(levels(SelectedVarDisc())))
      }else{
        pieDiscML <- ModelsDiscret$outcorHMM[[Dimod[1]]]$states
      }
      
      if (input$bestState == 1){
        nn <- apply(pieDiscML, 1,function(x) which(x == max(x)))
        pieDiscML[,1:ncol(pieDiscML)] <- 0 
        
        for (i in 1:length(nn)) {
          pieDiscML[i,nn[[i]]] <- 1
        }
      }
      
      if (length(Dimod) > 0){
        plotTree.datamatrix(treeInput(),as.data.frame(setNames(SelectedVarDisc(),row.names(CharInput()))),colors=list(DisCols),header=FALSE,fsize=0.45)
        
        legend('topright',legend = levels(SelectedVarDisc()),pch = 22,pt.cex=1.5, pt.bg = DisCols, bty='n',cex = 0.8)
        
        nodelabels(pie=pieDiscML,piecol=DisCols,cex=0.5)
      }
      
    }
    
  }else{
    DisColPal <- paletteer::paletteer_c("grDevices::Purple-Yellow", length(levels(SelectedVarDisc())))
    
    DisCols <- setNames(DisColPal,levels(SelectedVarDisc()))
    
    plotTree.datamatrix(treeInput(),as.data.frame(setNames(SelectedVarDisc(),row.names(CharInput()))),colors=list(DisCols),header=FALSE,fsize=0.45)
    
    legend('topright',legend = levels(SelectedVarDisc()),pch = 22,pt.cex=1.5, pt.bg = DisCols, bty='n',cex = 0.8)
    
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
        
        Dimod <-which(names(ModelsDiscret$outcorHMM) %in% input$SetModAverDisML)
        
        for (i in 1:length(input$SetModAverDisML)){
          
          plot(as.Qmatrix.corhmm(ModelsDiscret$outcorHMM[[Dimod[i]]],SelectedVarDisc()),main= input$SetModAverDisML[i],show.zeros=FALSE)
          
        }
        
      }else{
        
        par(mfrow = c(1,1),mar=c(1,1,1,1))
        Dimod <-which(names(ModelsDiscret$outcorHMM) == input$plotModelDisML)
        
        if (length(Dimod) > 0){
          plot(as.Qmatrix.corhmm(ModelsDiscret$outcorHMM[[Dimod]],SelectedVarDisc()),main= input$plotModelDisML[1],show.zeros=FALSE)
        }
      }
      
    }else{
      
      if (length(ModelsDiscret$multiStatesModels) > 1){
        par(mfrow = c(1,1))
        part <- ceiling(length(ModelsDiscret$multiStatesModels)/2)
        par(mfrow = c(part,2),mar=c(1,1,1,1))
        
        for (i in 1:length(ModelsDiscret$multiStatesModels)){
          plot(as.Qmatrix.matrix(ModelsDiscret$multiStatesModels[[i]],SelectedVarDisc()),main= names(ModelsDiscret$multiStatesModels)[i],show.zeros=FALSE)
        }
      }else{
        par(mfrow = c(1,1),mar=c(1,1,1,1))
        
        for (i in 1:length(ModelsDiscret$multiStatesModels)){
          
          plot(as.Qmatrix.matrix(ModelsDiscret$multiStatesModels[[i]],SelectedVarDisc()),main= names(ModelsDiscret$multiStatesModels)[i],show.zeros=FALSE)
        }
      }
    }
    
  })
})


#Update models to plot

observeEvent(input$RunAnalyDisML,{
  
  updateSelectInput(session, "plotModelDisML",choices=names(ModelsDiscret$outcorHMM))
})


observeEvent(input$RunModAverDisML,{
  updateSelectInput(session, "plotModelDisML",
                    choices=c(names(ModelsDiscret$outcorHMM),'ModelAverage'),selected = 'ModelAverage')
  
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
      utils::capture.output(ModelsDiscret$modelAverage,file = file)
    }else if (input$exportDisMLanc== "MLancRDS" & input$disCharOutputs == "ModelAverage"){
      saveRDS(object = ModelsDiscret$modelAverage,file = file)
    }else if (input$exportDisMLanc== 'MLancTXT' & input$disCharOutputs == "AICmatrix"){
      utils::capture.output(ModelsDiscret$AICdf,file = file)
    }else if (input$exportDisMLanc== 'MLancRDS' & input$disCharOutputs == "AICmatrix"){
      saveRDS(object = ModelsDiscret$AICdf,file = file)
    }else if (input$exportDisMLanc== 'MLancTXT' & input$disCharOutputs == "FittedModels"){
      utils::capture.output(ModelsDiscret$outcorHMM,file = file)
    }else if (input$exportDisMLanc== 'MLancRDS' & input$disCharOutputs == "FittedModels"){
      saveRDS(object = ModelsDiscret$outcorHMM,file = file)
    }
  }
)
