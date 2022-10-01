## ----setup, include=FALSE--------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------------------
library("readxl")
data_calf<-read_excel("California_Houses.xlsx")
data_calf=transform(data_calf[,-1], Median_Income=as.numeric( Median_Income),Distance_to_coast=as.numeric(Distance_to_coast),
                    Distance_to_LA=as.numeric(Distance_to_LA),Distance_to_SanJose=as.numeric(Distance_to_SanJose),
                    Distance_to_SanFrancisco=as.numeric(Distance_to_SanFrancisco),Distance_to_SanDiego=as.numeric(Distance_to_SanDiego))
summary(data_calf)


## --------------------------------------------------------------------------------------------------------------------
data_calf.cr<-scale(data_calf,center=T,scale=T)
summary(data_calf.cr)



## --------------------------------------------------------------------------------------------------------------------

inertie.expl <- rep(0,times=35)
k=2
clus <- kmeans(data_calf.cr,centers=k,nstart=5)
inertie.expl[k] <- clus$betweenss/clus$totss
while(inertie.expl[k]<0.95){
  k=k+1
  clus <- kmeans(data_calf.cr,centers=k,nstart=5)
  inertie.expl[k] <- clus$betweenss/clus$totss
}
max(inertie.expl)
k-1


## --------------------------------------------------------------------------------------------------------------------
plot(1:k,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")



## --------------------------------------------------------------------------------------------------------------------
n=1
regle=var(na.omit(inertie.expl[n+1:k]))*(k-n-1)*100/(var(inertie.expl)*(k-1))
while(regle>5){
  regle=var(na.omit(inertie.expl[n+1:k]))*(k-n-1)*100/(var(inertie.expl)*(k-1))
  n=n+1
}
n
regle


## --------------------------------------------------------------------------------------------------------------------
library(FactoMineR)
#Les données ont été préalablement centrées et réduites sur Excel
R.scale <- function(Y){
  #centrage réduction d'une variable - sans utilisation de boucle for
  X<-Y
  for (k in 1:ncol(X)){
    i<-1
    n <- length(X[,k])
    moy <- mean(X[,k])
    et <- sqrt((n-1)/n*var(X[,k]))
    while(i<nrow(X)){
    X[i,k] <- (X[i,k]-moy)/et
    i<-i+1
    }
  }
  return(Y)
  
}
Y=R.scale(data_calf)
#CAH
Res<-HCPC(as.data.frame(data_calf),nb.clust=-1)
Res$data.clust


## --------------------------------------------------------------------------------------------------------------------
Res$desc.var$quanti.var


## --------------------------------------------------------------------------------------------------------------------
Res$desc.var$quanti


## --------------------------------------------------------------------------------------------------------------------
Res$call$bw.before.consol



## --------------------------------------------------------------------------------------------------------------------
Res$call$bw.after.consol


## ----pressure, echo=FALSE--------------------------------------------------------------------------------------------
plot(pressure)

