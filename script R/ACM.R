## --------------------------------------------------------------------------------------------------------------------

library(readxl)

data_house <- read_excel("California_Houses.xlsx")
data_house$Median_Income <- as.numeric(data_house$Median_Income)
data_house$Distance_to_coast <- as.numeric(data_house$Distance_to_coast)
data_house$Distance_to_LA <- as.numeric(data_house$Distance_to_LA)
data_house$Distance_to_SanDiego <- as.numeric(data_house$Distance_to_SanDiego)
data_house$Distance_to_SanJose <- as.numeric(data_house$Distance_to_SanJose)
data_house$Distance_to_SanFrancisco <- as.numeric(data_house$Distance_to_SanFrancisco)


## --------------------------------------------------------------------------------------------------------------------
#centrage réduction des données
data_house<-as.data.frame(scale(data_house,center=T,scale=T))
data_kmeans=matrix(nrow = 109, ncol = 12)
taux_inertie=matrix(nrow = 1, ncol = 12)
for(i in 1:ncol(data_house)){
km <- kmeans(data_house[,i],centers=3,nstart=5)
data_kmeans[,i] <- km$cluster
taux_inertie[,i]=km$betweenss/km$totss
}
#transformer matrice to date frame 
taux_inertie=as.data.frame(taux_inertie)
data_kmeans=as.data.frame(data_kmeans)
#copier les nomes des colonnes
names(data_kmeans)=names(data_house)
names(taux_inertie)=names(data_house)

data_kmeans[,1]=factor(data_kmeans[,1],labels = c('MH-','MH+','MH++'))
data_kmeans[,2]=factor(data_kmeans[,2],labels = c('MI-','MI+','MI++'))
data_kmeans[,3]=factor(data_kmeans[,3],labels = c('MA-','MA+','MA++'))
data_kmeans[,4]=factor(data_kmeans[,4],labels = c('TR-','TR+','TR++'))
data_kmeans[,5]=factor(data_kmeans[,5],labels = c('TB-','TB+','TB++'))
data_kmeans[,6]=factor(data_kmeans[,6],labels = c('P-','P+','P++'))
data_kmeans[,7]=factor(data_kmeans[,7],labels = c('H-','H+','H++'))
data_kmeans[,8]=factor(data_kmeans[,8],labels = c('DC-','DC+','DC++'))
data_kmeans[,9]=factor(data_kmeans[,9],labels = c('DL-','DL+','DL++'))
data_kmeans[,10]=factor(data_kmeans[,10],labels = c('DSD-','DSD+','DSD++'))
data_kmeans[,11]=factor(data_kmeans[,11],labels = c('DSJ-','DSJ+','DSJ++'))
data_kmeans[,12]=factor(data_kmeans[,12],labels = c('DSF-','DSF+','DSF++'))

#ecrire dans un fichier excel 
library(xlsx)
# Ecrire la prémiere table 
write.xlsx(data_house, file = "data_kmeans.xlsx",sheetName="les variables quantitatives", append=FALSE)
# Ajouter une deuxième table
write.xlsx(data_kmeans, file = "data_kmeans.xlsx", sheetName="les variables qualitatives ",append=TRUE)
# Ajouter une troisième table
write.xlsx(taux_inertie, file = "data_kmeans.xlsx",  sheetName="le taux de discrétisation de chaque transformation", append=TRUE)
#on a le taux de discrétisation est supérieur à 0.5. 


## --------------------------------------------------------------------------------------------------------------------
library(FactoMineR)
#Construction du tableau disjonctif complet
tabl_dis <- tab.disjonctif(data_kmeans)




## --------------------------------------------------------------------------------------------------------------------
table_freq=matrix(nrow = 12, ncol = 3)
table_freq[1,]=table(data_kmeans$Median_House_Value)
table_freq[2,]=table(data_kmeans$Median_Income)
table_freq[3,]=table(data_kmeans$Median_Age)
table_freq[4,]=table(data_kmeans$Tot_Rooms)
table_freq[5,]=table(data_kmeans$Tot_Bedrooms)
table_freq[6,]=table(data_kmeans$Population)
table_freq[7,]=table(data_kmeans$Households)
table_freq[8,]=table(data_kmeans$Distance_to_coast)
table_freq[9,]=table(data_kmeans$Distance_to_LA)
table_freq[10,]=table(data_kmeans$Distance_to_SanDiego)
table_freq[11,]=table(data_kmeans$Distance_to_SanJose)
table_freq[12,]=table(data_kmeans$Distance_to_SanFrancisco)
table_freq=as.data.frame(table_freq)
colnames(table_freq) <- c("-","+","++")
table_freq=table_freq/109
print(colSums(table_freq < 0.01))
#n'existe aucune modalité rare 


## --------------------------------------------------------------------------------------------------------------------

res<-MCA(data_kmeans,ncp=6,graph=FALSE)
#le nombre d’individus
nrow(res$ind$coord)
#le nombre de variables
nrow(res$var$eta2)
#le nombre de modalités
nrow(res$var$coord)



## --------------------------------------------------------------------------------------------------------------------
#les valeurs propres
res$eig[,1]
# le pourcentage d’inertie de chaque valeur propre
res$eig[,2]
#le cumul des pourcentages d’inertie
res$eig[,3]



## --------------------------------------------------------------------------------------------------------------------

plot(res$eig[,1],type="b",ylab="Valeurs propres",xlab="Composantes",main="graphique des valeurs propres")
barplot(res$eig[,1],main="Eigenvalues", names.arg=paste("dim",1:nrow(res$eig)))


## --------------------------------------------------------------------------------------------------------------------
nrow(res$eig)
var(res$eig[1:24,1])*23/(var(res$eig[,1])*23)
#supérieur à 0,05 
var(res$eig[2:24,1])*22/(var(res$eig[,1])*23)
#supérieur à 0,05
var(res$eig[3:24,1])*21/(var(res$eig[,1])*23)
#supérieur à 0,05
var(res$eig[4:24,1])*20/(var(res$eig[,1])*23)
#supérieur à 0,05
var(res$eig[5:24,1])*19/(var(res$eig[,1])*23)
#supérieur à 0,05
var(res$eig[6:24,1])*18/(var(res$eig[,1])*23)
#supérieur à 0,05
var(res$eig[7:24,1])*17/(var(res$eig[,1])*23)
#inférieur à 0,05 
#Le sous espace de projection est constitué des 6 premiers axes


## --------------------------------------------------------------------------------------------------------------------
print(res$var$cos2,digit=2)


## --------------------------------------------------------------------------------------------------------------------
print(t(apply(res$var$cos2,1,cumsum)),digit=2)


## --------------------------------------------------------------------------------------------------------------------
cos_totale<-sort(t(apply(res$var$cos2,1,cumsum))[,6],decreasing=TRUE)
barplot(cos_totale,ylab="Qualite de representation",main="Cos2 total des variables jusqu'a Dim.6")



## --------------------------------------------------------------------------------------------------------------------
for (i in 1:36){
if (0.7 < cos_totale[i] ) print(cos_totale[i])
}


## --------------------------------------------------------------------------------------------------------------------

for (i in 1:36){
if (0.5<cos_totale[i]&& cos_totale[i]<0.7) print(cos_totale[i])
}



## --------------------------------------------------------------------------------------------------------------------
for (i in 1:36){
if ( cos_totale[i]< 0.5) print(cos_totale[i])
}



## --------------------------------------------------------------------------------------------------------------------
contrib<-data.frame(res$var$contrib)
print(res$var$contrib,digit=2)


## --------------------------------------------------------------------------------------------------------------------
cah<-HCPC(contrib,nb.clust=-1)
cah$desc.var



## --------------------------------------------------------------------------------------------------------------------
plot(res,invisible="ind")



## --------------------------------------------------------------------------------------------------------------------
print(res$ind$cos2,digit=2)



## --------------------------------------------------------------------------------------------------------------------
head(t(apply(res$ind$cos2,1,cumsum)),digit=2)


## --------------------------------------------------------------------------------------------------------------------
cos_totale_ind<-sort(t(apply(res$ind$cos2,1,cumsum))[,6],decreasing=TRUE)
barplot(cos_totale_ind,ylab="Qualite de representation",main="Cos2 total d'individus jusqu'a Dim.6")


## --------------------------------------------------------------------------------------------------------------------
for (i in 1:109){
if (0.7 < cos_totale_ind[i] ) print(cos_totale_ind[i])
}



## --------------------------------------------------------------------------------------------------------------------
for (i in 1:109){
if (0.5<cos_totale_ind[i]&& cos_totale_ind[i]<0.7) print(cos_totale_ind[i])
}



## --------------------------------------------------------------------------------------------------------------------
for (i in 1:109){
if ( cos_totale_ind[i]< 0.5) print(cos_totale_ind[i])
}



## --------------------------------------------------------------------------------------------------------------------
contrib_ind<-data.frame(res$ind$contrib)
print(res$ind$contrib,digit=2)



## --------------------------------------------------------------------------------------------------------------------
cah_ind<-HCPC(contrib_ind,nb.clust=-1)
cah_ind$desc.var



## --------------------------------------------------------------------------------------------------------------------
res$var$eta2


## --------------------------------------------------------------------------------------------------------------------
plot(res,choix="var")

