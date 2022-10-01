## ----setup, include=FALSE--------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------------------
library("readxl")
data_calf<-read_excel("California_Houses.xlsx")


## --------------------------------------------------------------------------------------------------------------------
data_calf=transform(data_calf, Median_Income=as.numeric( Median_Income),
                    Distance_to_coast=as.numeric(Distance_to_coast),
                    Distance_to_LA=as.numeric(Distance_to_LA),
                    Distance_to_SanJose=as.numeric(Distance_to_SanJose),
                    Distance_to_SanFrancisco=as.numeric(Distance_to_SanFrancisco),
                    Distance_to_SanDiego=as.numeric(Distance_to_SanDiego))
summary(data_calf)


## --------------------------------------------------------------------------------------------------------------------
regression_multiple<-lm(Median_House_Value ~ Median_Income + Median_Age + Tot_Rooms + Tot_Bedrooms + 
                         Population+ Households + Distance_to_coast + Distance_to_LA  +                                       Distance_to_SanDiego + Distance_to_SanJose +
                         Distance_to_SanFrancisco,data=data_calf)
summary(regression_multiple)



## --------------------------------------------------------------------------------------------------------------------
reg_ameliore=step(regression_multiple)
modele<-lm(Median_House_Value ~ Median_Income + Tot_Rooms + 
              Tot_Bedrooms + Population + Distance_to_coast + Distance_to_LA + 
              Distance_to_SanDiego + Distance_to_SanJose + Distance_to_SanFrancisco, 
            data = data_calf)
summary(modele)




## --------------------------------------------------------------------------------------------------------------------
par(mfrow = c(1, 2))
plot(reg_ameliore,1)
plot(reg_ameliore,3)



## --------------------------------------------------------------------------------------------------------------------
#test shapiro
shapiro.test(reg_ameliore$residuals)

#test ks
ks.test(reg_ameliore$residuals,"pnorm")



## --------------------------------------------------------------------------------------------------------------------
rse=sqrt(deviance(reg_ameliore)/df.residual(reg_ameliore))
abr=abs(data_calf$Median_House_Value-predict(reg_ameliore))/rse
plot(abr)
abline(h=2,col='red')



## --------------------------------------------------------------------------------------------------------------------
nva=ncol(data_calf)
Fish = rep(0,nva)
for (i in  2:ncol(data_calf)){
mod1<-lm(data_calf[,1]~data_calf[,i])
Fish[i]=var(predict(mod1))*(nrow(data_calf)-1)/(deviance(mod1)/df.residual(mod1))
}
Fish
df2=nrow(data_calf)-2
df2
1-pf(max(Fish),1,df2)


## --------------------------------------------------------------------------------------------------------------------
nva=ncol(data_calf)-1
Fish = rep(0,nva)
SCR1<-deviance(lm(data_calf[,1]~data_calf[,2]))
for (i in 3:ncol(data_calf)) {
 mod<-lm(data_calf[,1]~data_calf[,2]+data_calf[,i])
SCR2=deviance(mod)
Fish[i]=(SCR1-SCR2)/(SCR2/(nrow(data_calf)-3))
}
Fish
df2=nrow(data_calf)-3
df2
1-pf(max(Fish),1,df2)
summary(mod)


## --------------------------------------------------------------------------------------------------------------------
Fish = rep(0,2)
SCR2=deviance(lm(data_calf[,1]~data_calf[,2]+data_calf[,8]))
mod<-lm(data_calf[,1]~data_calf[,2])
SCR1<-deviance(mod)
Fish[1]=(SCR1-SCR2)/(SCR2/(nrow(data_calf)-3))

mod<-lm(data_calf[,1]~data_calf[,8])
SCR1=deviance(mod)
Fish[2]=(SCR1-SCR2)/(SCR2/(nrow(data_calf)-3))
Fish
df2=nrow(data_calf)-3
df2
1-pf(min(Fish),1,df2)


## --------------------------------------------------------------------------------------------------------------------
Fish = rep(0,nva)
SCR2<-deviance(lm(data_calf[,1]~data_calf[,2]+data_calf[,8]))
for (i in 3:7) {
 mod<-lm(data_calf[,1]~data_calf[,2]+data_calf[,8]+data_calf[,i])
SCR3=deviance(mod)
Fish[i]=(SCR2-SCR3)/(SCR3/(nrow(data_calf)-4))
}
for (i in 9:ncol(data_calf)) {
 mod<-lm(data_calf[,1]~data_calf[,2]+data_calf[,8]+data_calf[,i])
SCR3=deviance(mod)
Fish[i]=(SCR2-SCR3)/(SCR3/(nrow(data_calf)-4))
}
Fish
df2=nrow(data_calf)-4
df2
1-pf(max(Fish),1,df2)


## --------------------------------------------------------------------------------------------------------------------
SCR3<-deviance(lm(data_calf[,1]~data_calf[,2]+data_calf[,8]+data_calf[,9]))
Fish<-rep(0,3)
mod<-lm(data_calf[,1]~data_calf[,2]+data_calf[,8])
SCR2<-deviance(mod)
Fish[1]=(SCR2-SCR3)/(SCR3/(nrow(data_calf)-4))

mod<-lm(data_calf[,1]~data_calf[,2]+data_calf[,9])
SCR2<-deviance(mod)
Fish[2]=(SCR2-SCR3)/(SCR3/(nrow(data_calf)-4))

mod<-lm(data_calf[,1]~data_calf[,8]+data_calf[,9])
SCR2<-deviance(mod)
Fish[3]=(SCR2-SCR3)/(SCR3/(nrow(data_calf)-4))

Fish
df2=nrow(data_calf)-4
df2
1-pf(min(Fish),1,df2)


## --------------------------------------------------------------------------------------------------------------------
Fish = rep(0,nva)
SCR3<-deviance(lm(data_calf[,1]~data_calf[,2]+data_calf[,8]+data_calf[,9]))
for (i in 3:7) {
 mod<-lm(data_calf[,1]~data_calf[,2]+data_calf[,8]+data_calf[,9]+data_calf[,i])
SCR4=deviance(mod)
Fish[i]=(SCR3-SCR4)/(SCR4/(nrow(data_calf)-4))
}
for (i in 10:ncol(data_calf)) {
 mod<-lm(data_calf[,1]~data_calf[,2]+data_calf[,8]+data_calf[,9]+data_calf[,i])
SCR4=deviance(mod)
Fish[i]=(SCR3-SCR4)/(SCR4/(nrow(data_calf)-4))
}
Fish
df2=nrow(data_calf)-4
df2
1-pf(max(Fish),1,df2)


## --------------------------------------------------------------------------------------------------------------------
SCR4<-deviance(lm(data_calf[,1]~data_calf[,2]+data_calf[,8]+data_calf[,9]+data_calf[,10]))
Fish<-rep(0,4)
mod<-lm(data_calf[,1]~data_calf[,2]+data_calf[,8]+data_calf[,9])
SCR3<-deviance(mod)
Fish[1]=(SCR3-SCR4)/(SCR4/(nrow(data_calf)-4))

mod<-lm(data_calf[,1]~data_calf[,2]+data_calf[,8]+data_calf[,10])
SCR3<-deviance(mod)
Fish[2]=(SCR3-SCR4)/(SCR4/(nrow(data_calf)-4))

mod<-lm(data_calf[,1]~data_calf[,2]+data_calf[,9]+data_calf[,10])
SCR3<-deviance(mod)
Fish[3]=(SCR3-SCR4)/(SCR4/(nrow(data_calf)-4))

mod<-lm(data_calf[,1]~data_calf[,8]+data_calf[,9]+data_calf[,10])
SCR3<-deviance(mod)
Fish[4]=(SCR3-SCR4)/(SCR4/(nrow(data_calf)-4))



Fish
df2=nrow(data_calf)-4
df2
1-pf(min(Fish),1,df2)


## --------------------------------------------------------------------------------------------------------------------
Fish = rep(0,nva)
SCR4<-deviance(lm(data_calf[,1]~data_calf[,2]+data_calf[,8]+data_calf[,9]+data_calf[,10]))
for (i in 3:7) {
 mod<-lm(data_calf[,1]~data_calf[,2]+data_calf[,8]+data_calf[,9]+data_calf[,10]+data_calf[,i])
SCR5=deviance(mod)
Fish[i]=(SCR4-SCR5)/(SCR5/(nrow(data_calf)-4))
}
Fish[8]=Fish[9]=Fish[10]=0
for (i in 11:ncol(data_calf)) {
 mod<-lm(data_calf[,1]~data_calf[,2]+data_calf[,8]+data_calf[,9]+data_calf[,10]+data_calf[,i])
SCR5=deviance(mod)
Fish[i]=(SCR4-SCR5)/(SCR5/(nrow(data_calf)-4))
}
Fish
df2=nrow(data_calf)-4
df2
1-pf(max(Fish),1,df2)


## --------------------------------------------------------------------------------------------------------------------
model2<-lm(Median_House_Value ~ Median_Income + Distance_to_coast + Distance_to_LA + 
              Distance_to_SanDiego + Distance_to_SanJose ,
            data = data_calf)
par(mfrow = c(1, 2))
plot(model2,1)
plot(model2,3)



## --------------------------------------------------------------------------------------------------------------------
#test shapiro
shapiro.test(model2$residuals)

#test ks
ks.test(model2$residuals,"pnorm")



## --------------------------------------------------------------------------------------------------------------------
rse=sqrt(deviance(model2)/df.residual(model2))
#abr=abs(data$Median_House_Value-predict(model2))/rse
plot(as.numeric(abr))
abline(h=2,col='red')




## --------------------------------------------------------------------------------------------------------------------
AIC(model2)


## ----pressure, echo=FALSE--------------------------------------------------------------------------------------------
plot(pressure)

