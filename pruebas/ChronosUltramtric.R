library(phytools)
library(phangorn)
data("Laurasiatherian")


str(Laurasiatherian)
mp.tree<-pratchet(Laurasiatherian)
mp.tree$edge.length<-runif(n=nrow(mp.tree$edge))

lik.model<-pml(mp.tree,Laurasiatherian,k=4)

ml.fit<-optim.pml(lik.model,optGamma=TRUE,optBf=TRUE,optQ=TRUE,
                  rearrangement="ratchet")

ml.tree<-root(ml.fit$tree,outgroup="Platypus")
ml.tree<-drop.tip(ml.tree,"Platypus")
plotTree(ml.tree)

nodes<-c(findMRCA(ml.tree,c("Possum","Cat")),
         findMRCA(ml.tree,c("Squirrel","Mouse")),
         findMRCA(ml.tree,c("Pig","BlueWhale")),
         findMRCA(ml.tree,c("Human","Baboon")),
         findMRCA(ml.tree,c("Horse","Donkey")))
age.min=c(159,66,59,27.95,6.2)
age.max=c(166,75,66,31.35,10)
plotTree(ladderize(ml.tree))
obj<-get("last_plot.phylo",envir=.PlotPhyloEnv)
points(obj$xx[nodes],obj$yy[nodes],pch=21,bg=palette()[1:5],cex=2)
legend("bottomleft",paste("(",age.max,", ",age.min,") mya",sep=""),
       pch=21,pt.bg=palette()[1:5],pt.cex=2,bty="n")


calibration<-makeChronosCalib(ml.tree,node=nodes,
                              age.min=age.min,age.max=age.max)
calibration


pl.tree<-chronos(ml.tree,calibration=calibration,)


pl.tree

plotTree(pl.tree,direction="leftwards",xlim=c(174,-40),
         ftype="i",mar=c(4.1,1.1,0.1,1.1),fsize=0.8)
axis(1)
title(xlab="millions of years before present")
abline(v=seq(0,150,by=50),lty="dotted",col="grey")


ult <-is.ultrametric(ml.tree)

N <- Ntip(ml.tree)
root_nodes <- N+1
root_to_tips <- dist.nodes(ml.tree)[1:N,root_nodes]

min_tip <- min(root_to_tips)
max_tip <- max(root_to_tips)

max_tip/min_tip

scaled_root_to_tip <- root_to_tips * 1000


var(scaled_root_to_tip)
## [1] 6.519647e-07
min_tip <- min(scaled_root_to_tip)
max_tip <- max(scaled_root_to_tip)
(max_tip - min_tip) / max_tip



ml.tree2 <-CollapseNode(ml.tree, c(90, 91))


nodes<-c(findMRCA(ml.tree,c("Possum","Cat")),
         findMRCA(ml.tree,c("Squirrel","Mouse")),
         findMRCA(ml.tree,c("Pig","BlueWhale")),
         findMRCA(ml.tree,c("Human","Baboon")),
         findMRCA(ml.tree,c("Horse","Donkey")))
age.min=c(159,66,59,27.95,6.2)
age.max=c(166,75,66,31.35,10)
plotTree(ladderize(ml.tree))
obj<-get("last_plot.phylo",envir=.PlotPhyloEnv)
points(obj$xx[nodes],obj$yy[nodes],pch=21,bg=palette()[1:5],cex=2)
legend("bottomleft",paste("(",age.max,", ",age.min,") mya",sep=""),
       pch=21,pt.bg=palette()[1:5],pt.cex=2,bty="n")


calibration<-makeChronosCalib(ml.tree,node=nodes,
                              age.min=age.min,age.max=age.max)
calibration


pl.tree<-chronos(ml.tree,calibration=calibration,)







plot(pl.tree)

is.ultrametric(pl.tree)

write.tree( pl.tree,'Example.Ultrametric.phy')

saveRDS(pl.tree, 'Documents/Guane/data/Example.Ultrametric.RDS')



is.ultrametric(ml.tree2)

write.tree( ml.tree2,'Example.noUltrametric.phy')
saveRDS(ml.tree2, 'Documents/Guane/data/Example.noUltrametric.RDS')

is.binary(ml.tree2)




#########################################################################################



diver <- list()
diver$object <- list()

diver$object$yule <- list()

diver$object$yule$yule1 <- list()

diver$object$yule$yule1$Rho <- 0.1

diver$object$yule$yule1$Rate <- 0.2



diver$object$yule$yule2 <- list()

diver$object$yule$yule2$Rho <- 0.1

diver$object$yule$yule2$Rate <- 0.1


diver$object$yule$yule3 <- list()

diver$object$yule$yule3$Rho <- 0.1

diver$object$yule$yule3$Rate <- 0.3


models <- c('yule1', 'yule3')

diver$object$yule[[1]]$Rho


 yulemodels <- which(names(diver$object$yule) %in%  models)


 for (i in 1:length(yulemodels)){
   
yule <-make.yule(tree = pl.tree, sampling.f = as.numeric(diver$object$yule[[yulemodels[i]]]$Rho), unresolved = NULL)

fityule <- find.mle(func = yule, x.init= as.numeric(diver$object$yule[[yulemodels[i]]]$Rate))

print(diver$object$yule[i])
print(fityule)


}

