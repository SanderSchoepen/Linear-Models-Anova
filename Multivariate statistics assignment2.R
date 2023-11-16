#Task 2

setwd("C:/Users/saaan/Downloads")


install.packages("HDclassif")
install.packages("mclust")
install.packages("CrossClustering")

library(car)
library(stats)
library(dplyr)
library(multcomp)
library(HDclassif)
library(mclust)
library(plyr)


#read data

load("fashion.rdata")
head(train.data)

#center data
train.data<-scale(train.data, center=TRUE, scale= FALSE)
head(train.data)
set.seed(1)

#HDDC model: AkjBkQkD

#matrix with rand index
AkjBkQkD<-rep(0,6)
#ARI for first common dimension not calculated
AkjBkQkD[1]<-NA

set.seed(1)
hddc2<-hddc(train.data,K=3, model=c("AkjBkQkD"), com_dim = 2,show = TRUE)
AkjBkQkD[2]<-adjustedRandIndex(hddc2$class,train.target)

set.seed(1)
hddc3<-hddc(train.data,K=3, model=c("AkjBkQkD"), com_dim = 3,show = TRUE)
AkjBkQkD[3]<-adjustedRandIndex(hddc3$class,train.target)

set.seed(1)
hddc4<-hddc(train.data,K=3, model=c("AkjBkQkD"), com_dim = 4,show = TRUE)
AkjBkQkD[4]<-adjustedRandIndex(hddc4$class,train.target)

set.seed(1)
hddc5<-hddc(train.data,K=3, model=c("AkjBkQkD"), com_dim = 5,show = TRUE)
AkjBkQkD[5]<-adjustedRandIndex(hddc5$class,train.target)

set.seed(1)
hddc6<-hddc(train.data,K=3, model=c("AkjBkQkD"), com_dim = 6,show = TRUE)
AkjBkQkD[6]<-adjustedRandIndex(hddc6$class,train.target)

#HDDC model: AkjBQkD

AkjBQkD<-rep(0,6)
#ARI for first common dimension not calculated
AkjBQkD[1]<-NA

set.seed(1)
hddcb2<-hddc(train.data,K=3, model=c("AkjBQkD"), com_dim = 2,show = TRUE)
AkjBQkD[2]<-adjustedRandIndex(hddcb2$class,train.target)

set.seed(1)
hddcb3<-hddc(train.data,K=3, model=c("AkjBQkD"), com_dim = 3,show = TRUE)
AkjBQkD[3]<-adjustedRandIndex(hddcb3$class,train.target)

set.seed(1)
hddcb4<-hddc(train.data,K=3, model=c("AkjBQkD"), com_dim = 4,show = TRUE)
AkjBQkD[4]<-adjustedRandIndex(hddcb4$class,train.target)

set.seed(1)
hddcb5<-hddc(train.data,K=3, model=c("AkjBQkD"), com_dim = 5,show = TRUE)
AkjBQkD[5]<-adjustedRandIndex(hddcb5$class,train.target)

set.seed(1)
hddcb6<-hddc(train.data,K=3, model=c("AkjBQkD"), com_dim = 6,show = TRUE)
AkjBQkD[6]<-adjustedRandIndex(hddcb6$class,train.target)

#create table with adjusted rand index per model and per common dimension

ari.hddc<-rbind(AkjBkQkD,AkjBQkD)
ari.hddc

dimnames(ari.hddc)<-list(c("AkjBkQkD","AkjBQkD"),paste(1:6,"dim"))


#Mclust

#compute the principal components
prcomp1<-prcomp(train.data)
#prcomp1
#find VAF by first two principal components
cumsum(prcomp1$sd^2/sum(prcomp1$sd^2))

comp<-as.matrix(train.data)%*%prcomp1$rotation
comp

#Find AIC
#VVE
vve<-rep(0,6)
vve

#loop
for (i in 2:6){
  set.seed(1)
mclust_i<-Mclust(comp[,1:i],G=3, modelNames ="VVE") 

vve[i]<-adjustedRandIndex(mclust_i$class,train.target)
}
vve
#Vev
vev<-rep(0,6)
vev

for (i in 2:6){
  set.seed(1)
  mclust_i<-Mclust(comp[,1:i],G=3, modelNames ="VEV") 
  
  vev[i]<-adjustedRandIndex(mclust_i$class,train.target)
}
vev

#EVV
evv<-rep(0,6)
evv

for (i in 2:6){
  set.seed(1)
  mclust_i<-Mclust(comp[,1:i],G=3, modelNames="EVV") 
  
  evv[i]<-adjustedRandIndex(mclust_i$class,train.target)
}
evv

#VVV
vvv<-rep(0,6)
vvv

for (i in 2:6){
  set.seed(1)
  mclust_i<-Mclust(comp[,1:i],G=3, modelNames ="VVV") 
  
  vvv[i]<-adjustedRandIndex(mclust_i$class,train.target)
  
}
vvv
vve[1]<-NA
vev[1]<-NA
evv[1]<-NA
vvv[1]<-NA


#combine vecotrs 

ari.Mclust<-rbind(vve,vev,evv,vvv)
ari.Mclust
dimnames(ari.Mclust)<-list(c("VVE", "VEV", "EVV", "VVV"),paste(1:6,"PC"))
ari.Mclust

#Fit the selected model
set.seed(1)
mclust_selected<-Mclust(comp[,1:6],G=3, modelNames="EVV") 
mclust_selected
#check if class labels need to be switched
tab<-table(mclust_selected$class,train.target)
tab
#class labels  need to be switched
ntab<-tab[c(3,1,2),]
ntab

#Principal component 
par(mfrow=c(1,2))
#true values

plot(comp,main="Observed clusters")
points(comp[train.target==0,1:2],col="black", pch=19)
points(comp[train.target==1,1:2],col="red", pch=19)
points(comp[train.target==7,1:2],col="blue", pch=19)
legend("bottomleft", c("T-shirt","Trouser","Sneaker"),
       col=c("black","red","blue"),pch=c(19,19,19),bty = "n")


#predicted values

plot(comp,main="Derived clusters")
points(comp[mclust_selected$class==3,1:2],col="black", pch=19)
points(comp[mclust_selected$class==1,1:2],col="red", pch=19)
points(comp[mclust_selected$class==2,1:2],col="blue", pch=19)
legend("bottomleft", c("T-shirt","Trouser","Sneaker"),
       col=c("black","red","blue"),pch=c(19,19,19),bty = "n")
