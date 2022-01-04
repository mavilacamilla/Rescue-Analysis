setwd("C:/")
library(adegenet)
library(spdep)
library(raster) 
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(reshape2) 
library(geosphere)
library(fuzzyjoin)  

data.pres<-read.csv("suit.pres.diutus.csv")
data.fut45<-read.csv("suit.fut45.diutus.csv")
data.fut85<-read.csv("suit.fut85.diutus.csv")
names(data.pres)<-c("idex","long","lat","suit")
names(data.fut45)<-c("idex","long","lat","suit")
names(data.fut85)<-c("idex","long","lat","suit")
temp.pres<-read.table("bio # CCSM_Modern(1950-1999).txt",h=T)
pres.bio8<-temp.pres[,c(2,3,11)]
temp.fut45<-read.table("bio #baseline_Modern(1950-1999)# CCSM_rcp45(2080-2100).txt",h=T)
fut45.bio8<-temp.fut45[,c(2,3,11)]
temp.fut85<-read.table("bio #baseline_Modern(1950-1999)# CCSM_rcp85(2080-2100).txt",h=T)
fut85.bio8<-temp.fut85[,c(2,3,11)]
temp_pres<-pres.bio8[,3]
temp_fut45<-fut45.bio8[,3]
temp_fut85<-fut85.bio8[,3]


data.occurence<-read.csv("diutius.csv")
split.occur<-colsplit(data.occurence$species.long.lat, pattern=";", names=paste0("sep", 1:3))
names(split.occur)<-c("specie","long","lat")
id<-1:22
occur.data<-data.frame(id,split.occur)  

#join occur.data and pres.bio8 by aproximate location
data.join<-geo_left_join(occur.data,pres.bio8,by=c("long","lat"),max_dist=28,method="haversine",distance_col="")
names(data.join)<-c("id","lemniscatus.occur","long.x","lat.x","long.y","lat.y","bio8","dist","nothing")

#remove repeated locations leaving only closest ones  
occur_order<-arrange(data.join,id,dist)      
occur.final<-distinct(occur_order,id, .keep_all = TRUE) 

#clean table
diutius_occurence<-occur.final[,c(1,2,5,6,7)]  
names(diutius_occurence)<-c( "id","lemniscatus","long","lat","bio8")

###
#Present
occurence.pres<-merge(diutius_occurence,data.pres,by=c("long","lat"))
threshold<-min(occurence.pres$suit)

dados.pres<-merge(data.pres,pres.bio8)
dados.fut45<-merge(data.fut45,fut45.bio8)
dados.fut85<-merge(data.fut85,fut85.bio8)

clima.pres<-dados.pres$bio.8
clima.fut45<-dados.fut45$bio.8
clima.fut85<-dados.fut85$bio.8


suit.pres<-dados.pres$suit
suit.fut45<-dados.fut45$suit
mean(suit.fut45)
suit.fut85<-dados.fut85$suit
mean(suit.fut85)

coord<-dados.pres[,c(1,2)]

distr.pres<-ifelse(suit.pres>threshold,1,0)
distr.fut45<-ifelse(suit.fut45>threshold,1,0)
distr.fut85<-ifelse(suit.fut85>threshold,1,0)


presence.pres<-which(distr.pres==1)
length(presence.pres)
presence.fut45<-which(distr.fut45==1)
length(presence.fut45)
presence.fut85<-which(distr.fut85==1)
length(presence.fut85)

distr_pres<-ifelse(suit.pres>threshold,suit.pres,0)
distr_fut45<-ifelse(suit.fut45>threshold,suit.fut45,0)
distr_fut85<-ifelse(suit.fut85>threshold,suit.fut85,0)


# Future Regions RCP4.5
stable.region45<-ifelse(distr.pres==1 & distr.fut45==1,1,0)
leading.edge45<-ifelse((distr.pres-distr.fut45)< 0,1,0)
trailing.edge45<-ifelse(distr.pres==1 & distr.fut45==0,1,0)
trailing.edge45.1<-which(trailing.edge45==1)
length(trailing.edge45.1)
range.total45<-ifelse((distr.pres+distr.fut45) >= 1,1,0) 
field45<-which(range.total45==1)
field45.size<-length(field45) 

ms <-cbind(range.total45*clima.fut45, range.total45*clima.pres)

suit.rg2 <-numeric()
for(i in 1:length(clima.pres)){
  suit.rg2[i] <-max(ms[i,1],ms[i,2])
}
hist(suit.rg2)
suit0.field <-suit.rg2[field45]


#Future Regions RCP8.5
stable.region85<-ifelse(distr.pres==1 & distr.fut85==1,1,0)
leading.edge85<-ifelse((distr.pres-distr.fut85)< 0,1,0)
trailing.edge85<-ifelse(distr.pres==1 & distr.fut85==0,1,0)
trailing.edge85.1<-which(trailing.edge85==1)
length(trailing.edge85.1)
range.total85<-ifelse((distr.pres+distr.fut85) >= 1,1,0) 
field85<-which(range.total45==1)
field85.size<-length(field45) 

ms.85 <-cbind(range.total85*clima.fut85, range.total85*clima.pres)

suit.rg2 <-numeric()
for(i in 1:length(clima.pres)){
  suit.rg2[i] <-max(ms.85[i,1],ms.85[i,2])
}
hist(suit.rg2)
suit0.field.85 <-suit.rg2[field85]


### Temperature Analysis
#Present
min(clima.pres[presence.pres])
max(clima.pres[presence.pres])
mean(clima.pres[presence.pres])
sd(temp_pres[presence.pres])
hist.temp.pres<-hist(clima.pres[presence.pres],breaks=pretty(20:30,n=15),col="gray70",main="",xlim=c(20,30),xlab=" Current mean temperature of wettest quarter (worldclim Bio8)",ylab="Cells")


#Future RCP4.5
min(clima.fut45[presence.fut45])
max(clima.fut45[presence.fut45])
mean(clima.fut45[presence.fut45])
hist.temp.fut45<-hist(clima.fut45[presence.fut45],breaks=pretty(20:30,n=15),col="gray70",main="",xlim=c(20,30),xlab=" Current mean temperature of wettest quarter (worldclim Bio8)",ylab="Cells")

#Future RCP8.5
min(clima.fut85[presence.fut85])
max(clima.fut85[presence.fut85])
mean(clima.fut85[presence.fut85])
hist.temp.fut85<-hist(clima.fut85[presence.fut85],breaks=pretty(20:30,n=15),col="gray70",main="",xlim=c(20,30),xlab=" Current mean temperature of wettest quarter (worldclim Bio8)",ylab="Cells")

pretty(20:30,n=5)

#Diference Pres/Fut45
plot(hist.temp.pres, col=rgb(0,0,0,1/2),
     main = "Present and RCP4.5" ,
     xlab = "Mean Temperature of Wettest Quarter(Bio8)",
     ylab = "Cells",
     ylim =c(0,400),
     xlim=c(20,30))
plot(hist.temp.fut45, col=rgb(1,1,1,1/3), add=T)
abline(v=mean(clima.pres[presence.pres]),col="green")
abline(v=mean(clima.fut45[presence.fut45]),col="red")

#Diference Pres/Fut85
plot(hist.temp.pres, col=rgb(0,0,0,1/2),
     main = "Present and RCP8.5" ,
     xlab = "Mean Temperature of Wettest Quarter(Bio8)",
     ylab = "Cells",
     ylim =c(0,400),
     xlim=c(20,30))
plot(hist.temp.fut85, col=rgb(1,1,1,1/2), add=T)
abline(v=mean(clima.pres[presence.pres]),col="green")
abline(v=mean(clima.fut85[presence.fut85]),col="red")


#Maps

map_pres <- rasterFromXYZ(xyz =  cbind(coord,distr.pres) , res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_pres, col = brewer.pal(n = 4, name = "Greys"))

map_fut45<-rasterFromXYZ(xyz =  cbind(coord,distr.fut45) , res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_fut45,col = brewer.pal(n = 4, name = "Greys")) 

map_fut85<-rasterFromXYZ(xyz =  cbind(coord,distr.fut85) , res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_fut85,col = brewer.pal(n = 4, name = "Greys")) 

map_stable.region45<-rasterFromXYZ(xyz =  cbind(coord,stable.region45), res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_stable.region45,col = brewer.pal(n = 4, name = "Greys")) #Mapa do Stable Region RCP4.5 

map_trailing.edge45<-rasterFromXYZ(xyz =  cbind(coord,trailing.edge45), res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_trailing.edge45,col = brewer.pal(n = 4, name = "Greys")) #Mapa do Trailing Edge RCP4.5 

map_leading.edge45<-rasterFromXYZ(xyz =  cbind(coord,leading.edge45), res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_leading.edge45,col = brewer.pal(n = 4, name = "Greys")) #Mapa do Trailing Edge RCP4.5 map_trailing.edge45<-rasterFromXYZ(xyz =  cbind(coord,trailing.edge45), res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

map_stable.region85<-rasterFromXYZ(xyz =  cbind(coord,stable.region85), res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_stable.region85,col = brewer.pal(n = 4, name = "Greys")) #Mapa do Stable Region RCP4.5 

map_trailing.edge85<-rasterFromXYZ(xyz =  cbind(coord,trailing.edge85), res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_trailing.edge85,col = brewer.pal(n = 4, name = "Greys")) #Mapa do Trailing Edge RCP4.5 

map_leading.edge85<-rasterFromXYZ(xyz =  cbind(coord,leading.edge85), res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_leading.edge85,col = brewer.pal(n = 4, name = "Greys")) #Mapa do Trailing Edge RCP4.5 map_trailing.edge45<-rasterFromXYZ(xyz =  cbind(coord,trailing.edge45), res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


###Rescue  Futuro RCP4.5

#Haldane Futuro RCP4.5
DeltaT45<-clima.fut45[which(trailing.edge45==1)]-clima.pres[which(trailing.edge45==1)]
mean(DeltaT45)

#Standarts deviations
sd1<-sd(clima.pres[presence.pres])
sd2<-sd(diutius_occurence$bio8) 
sd3<-(max(clima.pres[presence.pres])-min(clima.pres[presence.pres]))/5  

genaration<-90/2.26

g45<- runif(field45.size,30,40)

w2<- ((sd1+sd2)/2)^2*0.3*50

plast<-(26.9-21.2)/5 

rho <- 0.9 #autoregressive coefficient
neigh <-10 #neighbour size
longF <-dados.fut45[field45,1]
latF <-dados.fut45[field45,2]
coordsF <-cbind(longF,latF) 
dnb <- dnearneigh(coordsF, 0, neigh)
dsts <- nbdists(dnb, coordsF)
idw <- lapply(dsts, function(x) 1/x)
lw <- nb2listw(dnb, glist=idw, style="W") 
inv <- invIrW(lw, rho)  #vector to multiply data...



#Simulations...
nsim <-5000
geo.adap <-matrix(0,nrow(coord),nsim)
geo.adapP <-matrix(0,nrow(coord),nsim)
geo.hald <-matrix(0,nrow(coord),nsim)
out <-matrix(0,nsim,10)

for(i in 1:nsim){
  
  
  h2 <-runif(field45.size,0.20,0.40)
  FST <-runif(field45.size,0.1,0.25) 
  sdT<-runif(field45.size,sd1,sd3) 
  g1 <- runif(field45.size,30,40) 
  bp45<-runif(field45.size,0.00001,(plast^2/(mean(sdT))^2))  
  
  #Creating regional patterns in parameters
  h2.geo <- inv%*%h2
  sdT.geo <- inv%*%sdT
  FST.geo <- inv%*%FST
  g.geo <- inv%*%g1
  bp.geo <- inv%*%bp45
  
  
  
  #Rescaling to previous ranges	
  h2 <-(((0.4 - 0.2)*(h2.geo - min(h2.geo)))/ (max(h2.geo)-min(h2.geo))) + 0.2
  sdT0 <-(((sd2 - sd1)*(sdT.geo - min(sdT.geo)))/ (max(sdT.geo)- min(sdT.geo))) + sd1
  FST <-(((0.25 - 0.1)*(FST.geo - min(FST.geo)))/ (max(FST.geo)-min(FST.geo))) + 0.1
  g <-round(((40 - 30)*(g.geo - min(g.geo)))/ (max(g.geo)-min(g.geo))) + 40
  bp <-(((0.15 - 0.1)*(bp.geo - min(bp.geo)))/ (max(bp.geo)-min(bp.geo))) + 0.1
  
  #lambda<-runif(field45.size,1.04,1.12)
  lambda <-(((1.12 - 1.04)*(suit0.field - min(suit0.field)))/ (max(suit0.field)-min(suit0.field))) + 1.09
  
  #Model parameters	
  sdT1 <-sqrt((1 - FST) * (sdT0*sdT0))
  Vs <- w2 + ((sdT0*sdT0)-(h2*(sdT0*sdT0)))
  vT <-sdT1*sdT1
  vA <-vT*h2
  GT <-90/g45
  
  Haldane45<-((clima.fut45[field45]-clima.pres[field45])/sdT1)/g45 
  mean(clima.fut45[field45]-clima.pres[field45])
  mean(clima.fut85[field85]-clima.pres[field85])
  
  
  HDC.45 <-(vA*(sqrt(2*log(lambda*sqrt(w2/(vA+Vs)))/(vA+Vs))))/sqrt(vT) 
  
  
  log(x = 10,base = exp(1)) 
  HDP.45 <-sqrt(((2*log(lambda)*(1/Vs))/GT)*((h2*(sdT*sdT))/(1 - bp)))
  
  HDC.45[is.na(HDC.45)] <- 0
  HDP.45[is.na(HDP.45)] <- 0
  
  
  
  adapC.45 <-(ifelse(Haldane45 < HDC.45,1,0)) 
  resultC.45<-which(adapC.45==1)
  #tamanhoC<-length(result) 
  
  adapP.45<-(ifelse(Haldane45 < HDP.45,1,0)) 
  resultP.45<-which(adapP.45==1)
  
  #Realigning the vector
  
  hald.Neo <-matrix(0,length(range.total45),1)
  for(j in 1:length(adapC.45)){
    hald.Neo[field45[j]] <- Haldane45[j]
  }
  
  
  adap.Neo <-matrix(0,length(range.total45),1)
  for(j in 1:length(adapC.45)){
    adap.Neo[field45[j]] <-adapC.45[j]
  }
  adapP.Neo <-matrix(0,length(range.total45),1)
  for(j in 1:length(adapC.45)){
    adapP.Neo[field45[j]] <-adapP.45[j]
  }
  
  HDC.45.Neo <-matrix(0,length(range.total45),1)
  for(j in 1:length(HDC.45)){
    HDC.45.Neo[field45[j]] <- HDC.45[j]
  }
  
  HDP.45.Neo <-matrix(0,length(range.total45),1)
  for(j in 1:length(HDP.45)){
    HDP.45.Neo[field45[j]] <- HDP.45[j]
  }
  
  
  #Outputs...
  out[i,1] <-mean(HDC.45.Neo[which(trailing.edge45==1)])
  out[i,2] <-mean(HDP.45.Neo[which(trailing.edge45==1)])
  out[i,3] <-mean(hald.Neo[which(trailing.edge45==1)])
  out[i,4] <-mean(hald.Neo[which(stable.region45==1)])
  out[i,5] <-sum(adap.Neo*range.total45)/sum(range.total45)
  out[i,6] <-sum(adapP.Neo*range.total45)/sum(range.total45)
  out[i,7] <-sum(adap.Neo*trailing.edge45)/sum(trailing.edge45)
  out[i,8] <-sum(adapP.Neo*trailing.edge45)/sum(trailing.edge45)
  out[i,9] <-sum(adap.Neo*distr.fut45)/sum(distr.fut45)
  out[i,10] <-sum(adapP.Neo*distr.fut45)/sum(distr.fut45)
  
  
  
  #map of all species' field
  geo.adap[,i] <-adap.Neo 
  geo.adapP[,i] <-adapP.Neo
  geo.hald[,i] <-hald.Neo 
}


hist(out[,3],xlab="Mean Haldanes in trailing edge",ylab="Number of simulations",nclass=100,col="gray",main="") #Midia de Haldanes no Trailing Edge
t.test(out[,4],out[,3]) 
mean(out[,3]) 
sd(out[,3])

hist(out[,1],xlab="Mean MSER in trailing edge",ylab="Number of simulations",nclass=100,col="gray",main="",ylim=c(0,150))
mean(out[,1])
sd(out[,1])

hist(out[,2],xlab="Mean MSERp in trailing edge",ylab="Number of simulations",nclass=100,col="gray",main="",ylim=c(0,120))
mean(out[,2])
sd(out[,2])


hist(out[,7],xlab="Proportion of trailing edge rescued",ylab="Number of simulations",nclass=100,col="gray",main="")
hist(out[,8],xlab="Proportion of trailing edge rescued",ylab="Number of simulations",nclass=100,col="gray",main="") #MSERp no Trailing
median(out[,8])
max(out[,7])

hist(out[,5],xlab="Proportion of trailing edge rescued",ylab="Number of simulations",nclass=100,col="grey",main="")
hist(out[,9],xlab="Proportion of trailing edge rescued",ylab="Number of simulations",nclass=100,col="grey",main="")

#synthetizing among simulations...
sum(ifelse(out[,7] == 0.0,1,0))/nsim
sum(ifelse(out[,7] > 0.050,1,0))/nsim

sum(ifelse(out[,8] > 0.05,1,0))/nsim
sum(ifelse(out[,8] > 0.02,1,0))/nsim

#in the future region...
sum(ifelse(out[,9] > 0.050,1,0))/nsim
sum(ifelse(out[,9] == 0.0,1,0))/nsim
hist(out[,9])
max(out[,5])

#map analysis
map.adap <-apply(geo.adap,1,mean)
map.plast <-apply(geo.adapP,1,mean) 

#Mapa Adap s/plast RCP.4.5 (Trailing)
p.adap<-(apply(geo.adap,1,mean))*trailing.edge45 
a45<-p.adap+distr.fut45
map_adap <- rasterFromXYZ(xyz =  cbind(coord,a45) , res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_adap, col = brewer.pal(n = 4, name = "Greys")) 

#Mapa Adap c/plast RCP4.5  (Trailing)
p.adapP<-(apply(geo.adapP,1,mean))*trailing.edge45 
a45p<-p.adapP+distr.fut45
map_adapP <- rasterFromXYZ(xyz =  cbind(coord,a45p) , res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_adapP, col = brewer.pal(n = 4, name = "Greys")) 


#trailing
median(map.adap[which(trailing.edge45==1)])
hist(map.adap[which(trailing.edge45==1)])
sum(ifelse(map.adap[which(trailing.edge45==1)]>0.05,1,0))/sum(trailing.edge45)
median(map.plast[which(trailing.edge45==1)])
hist(map.plast[which(trailing.edge45==1)])

#future range
median(map.adap[which(distr.fut45==1)])
hist(map.adap[which(distr.fut45==1)])
sum(ifelse(map.adap[which(distr.fut45==1)]>0.05,1,0))/sum(distr.fut45)

median(map.plast[which(distr.fut45==1)])
hist(map.plast[which(distr.fut45==1)])

#species field
median(map.adap[which(range.total45==1)])
hist(map.adap[which(range.total45==1)])
median(map.plast[which(range.total45==1)])
hist(map.plast[which(range.total45==1)])


###Rescue para Futuro RCP8.5

#Haldane Future RCP8.5
DeltaT85<-clima.fut85[which(trailing.edge85==1)]-clima.pres[which(trailing.edge85==1)]
mean(DeltaT85)

#Standarts deviations 
sd1<-sd(clima.pres[presence.pres])
sd2<-sd(diutius_occurence$bio8) 
sd3<-(max(clima.pres[presence.pres])-min(clima.pres[presence.pres]))/5  
((sd1+sd2)/2)^2

genaration<-90/2.26

g85<- runif(field85.size,30,40)

w2<- (sd2^2)*20
plast<-(26.9-21.2)/5 

rho <- 0.9 #autoregressive coefficient
neigh <-10 #neighbour size
longF.85 <-dados.fut85[field85,1]
latF.85 <-dados.fut85[field85,2]
coordsF.85 <-cbind(longF.85,latF.85) 
dnb.85 <- dnearneigh(coordsF.85, 0, neigh)
dsts.85 <- nbdists(dnb.85, coordsF.85)
idw.85 <- lapply(dsts.85, function(x) 1/x)
lw.85 <- nb2listw(dnb.85, glist=idw.85, style="W") 
inv.85<- invIrW(lw.85, rho)  #vector to multiply data...


#Simulations...
nsim <-5000
geo.adap2 <-matrix(0,nrow(coord),nsim)
geo.adapP2 <-matrix(0,nrow(coord),nsim)
geo.hald2 <-matrix(0,nrow(coord),nsim)
out2 <-matrix(0,nsim,10)

for(i in 1:nsim){
  
  
  h2.85 <-runif(field85.size,0.20,0.40)
  FST.85 <-runif(field85.size,0.1,0.25) 
  sdT.85<-runif(field85.size,sd1,sd3) 
  g1.85 <- runif(field85.size,30,40) 
  bp85<-runif(field85.size,0.00001,(plast^2/(mean(sdT.85))^2))  
  
  #Creating regional patterns in parameters
  h2.geo.85 <- inv.85%*%h2.85
  sdT.geo.85 <- inv.85%*%sdT.85
  FST.geo.85 <- inv.85%*%FST.85
  g.geo.85<- inv.85%*%g1.85
  bp.geo.85 <- inv.85%*%bp85
  
  
  
  #Rescaling to previous ranges	
  h2.85<-(((0.4 - 0.2)*(h2.geo.85 - min(h2.geo.85)))/ (max(h2.geo.85)-min(h2.geo.85))) + 0.2
  sdT0.85<-(((sd2 - sd1)*(sdT.geo.85 - min(sdT.geo.85)))/ (max(sdT.geo.85)- min(sdT.geo.85))) + sd1
  FST.85<-(((0.25 - 0.1)*(FST.geo.85 - min(FST.geo.85)))/ (max(FST.geo.85)-min(FST.geo.85))) + 0.1
  g85 <-round(((40 - 30)*(g.geo.85 - min(g.geo.85)))/ (max(g.geo.85)-min(g.geo.85))) + 40
  bp85 <-(((0.15 - 0.1)*(bp.geo.85 - min(bp.geo.85)))/ (max(bp.geo.85)-min(bp.geo.85))) + 0.1
  
  #lambda<-runif(field45.size,1.04,1.12)
  lambda.85 <-(((1.12 - 1.04)*(suit0.field.85 - min(suit0.field.85)))/ (max(suit0.field.85)-min(suit0.field.85))) + 1.09
  
  #Model parameters	
  sdT1.85 <-sqrt((1 - FST.85) * (sdT0.85*sdT0.85))
  Vs.85 <- w2 + ((sdT0.85*sdT0.85)-(h2.85*(sdT0.85*sdT0.85)))
  vT.85 <-sdT1.85*sdT1.85
  vA.85 <-vT.85*h2.85
  GT.85<-90/g85
  
  Haldane85<-((clima.fut85[field85]-clima.pres[field85])/sdT1.85)/g85 
  mean(Haldane85)
  mean(Haldane45)
  
  
  HDC.85 <-(vA.85*(sqrt(2*log(lambda.85*sqrt(w2/(vA.85+Vs.85)))/(vA.85+Vs.85))))/sqrt(vT.85) 
  
  
  log(x = 10,base = exp(1)) 
  HDP.85 <-sqrt(((2*log(lambda.85)*(1/Vs.85))/GT.85)*((h2.85*(sdT.85*sdT.85))/(1 - bp85)))
  
  HDC.85[is.na(HDC.85)] <- 0
  HDP.85[is.na(HDP.85)] <- 0
  
  
  
  adapC.85 <-(ifelse(Haldane85 < HDC.85,1,0)) 
  resultC.85<-which(adapC.85==1)

  adapP.85<-(ifelse(Haldane85 < HDP.85,1,0)) 
  resultP.85<-which(adapP.85==1)
  
  #Realigning the vector
  
  hald.Neo2 <-matrix(0,length(range.total85),1)
  for(j in 1:length(adapC.85)){
    hald.Neo2[field85[j]] <- Haldane85[j]
  }
  
  
  adap.Neo2 <-matrix(0,length(range.total85),1)
  for(j in 1:length(adapC.85)){
    adap.Neo2[field85[j]] <-adapC.85[j]
  }
  adapP.Neo2 <-matrix(0,length(range.total85),1)
  for(j in 1:length(adapC.85)){
    adapP.Neo2[field85[j]] <-adapP.85[j]
  }
  
  HDC.85.Neo <-matrix(0,length(range.total85),1)
  for(j in 1:length(HDC.85)){
    HDC.85.Neo[field85[j]] <- HDC.85[j]
  }
  
  HDP.85.Neo <-matrix(0,length(range.total85),1)
  for(j in 1:length(HDP.85)){
    HDP.85.Neo[field85[j]] <- HDP.85[j]
  }
  
  
  #Outputs...
  out2[i,1] <-mean(HDC.85.Neo[which(trailing.edge85==1)])
  out2[i,2] <-mean(HDP.85.Neo[which(trailing.edge85==1)])
  out2[i,3] <-mean(hald.Neo2[which(trailing.edge85==1)])
  out2[i,4] <-mean(hald.Neo2[which(stable.region85==1)])
  out2[i,5] <-sum(adap.Neo2*range.total85)/sum(range.total85)
  out2[i,6] <-sum(adapP.Neo2*range.total85)/sum(range.total85)
  out2[i,7] <-sum(adap.Neo2*trailing.edge85)/sum(trailing.edge85)
  out2[i,8] <-sum(adapP.Neo2*trailing.edge85)/sum(trailing.edge85)
  out2[i,9] <-sum(adap.Neo2*distr.fut85)/sum(distr.fut85)
  out2[i,10] <-sum(adapP.Neo2*distr.fut85)/sum(distr.fut85)
  
  
  
  #map of all species' field
  geo.adap2[,i] <-adap.Neo2 
  geo.adapP2[,i] <-adapP.Neo2
  geo.hald2[,i] <-hald.Neo2 
}

hist(out2[,3],xlab="Mean Haldanes in trailing edge",ylab="Number of simulations",nclass=100,col="gray",main="") #Midia de Haldanes no Trailing Edge
t.test(out2[,4],out2[,3])
mean(out2[,3])
sd(out2[,3])

hist(out2[,1],xlab="Mean MSER in trailing edge",ylab="Number of simulations",nclass=100,col="gray",main="")
mean(out2[,1])
sd(out2[,1])

hist(out2[,2],xlab="Mean MSERp in trailing edge",ylab="Number of simulations",nclass=100,col="gray",main="")
mean(out2[,2])
sd(out2[,2])

hist(out2[,7],xlab="Proportion of trailing edge rescued",ylab="Number of simulations",nclass=100,col="gray",main="")
hist(out2[,8],xlab="Proportion of trailing edge rescued",ylab="Number of simulations",nclass=100,col="gray",main="") #MSERp no Trailing
median(out2[,8])
max(out2[,7])

hist(out2[,5],xlab="Proportion of trailing edge rescued",ylab="Number of simulations",nclass=100,col="grey",main="")
hist(out2[,9],xlab="Proportion of trailing edge rescued",ylab="Number of simulations",nclass=100,col="grey",main="")

#synthetizing among simulations...
sum(ifelse(out2[,7] == 0.0,1,0))/nsim
sum(ifelse(out2[,7] > 0.050,1,0))/nsim

sum(ifelse(out2[,8] > 0.05,1,0))/nsim

#in the future region...
sum(ifelse(out2[,9] > 0.050,1,0))/nsim
sum(ifelse(out2[,9] == 0.0,1,0))/nsim
hist(out2[,9])
max(out2[,5])

#map analysis
map.adap2<-apply(geo.adap2,1,mean)
map.plast2 <-apply(geo.adapP2,1,mean) 

#Mapa Adaptation without plasticity RCP.4.5 (Trailing)
p.adap2<-(apply(geo.adap2,1,mean))*trailing.edge85 
map_adap2 <- rasterFromXYZ(xyz =  cbind(coord,p.adap2) , res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_adap2, col = brewer.pal(n = 4, name = "PuBu")) 

#Mapa Adaptation with plasticity RCP4.5  (Trailing)
p.adapP2<-(apply(geo.adapP2,1,mean))*trailing.edge85 
map_adapP2 <- rasterFromXYZ(xyz =  cbind(coord,p.adapP2) , res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_adapP2, col = brewer.pal(n = 4, name = "PuBu")) 

#trailing
median(map.adap2[which(trailing.edge85==1)])
hist(map.adap2[which(trailing.edge85==1)])
sum(ifelse(map.adap2[which(trailing.edge85==1)]>0.05,1,0))/sum(trailing.edge85)
median(map.plast2[which(trailing.edge85==1)])
hist(map.plast2[which(trailing.edge85==1)])

#future range
median(map.adap2[which(distr.fut85==1)])
hist(map.adap2[which(distr.fut85==1)])
sum(ifelse(map.adap2[which(distr.fut85==1)]>0.05,1,0))/sum(distr.fut85)

median(map.plast2[which(distr.fut85==1)])
hist(map.plast2[which(distr.fut85==1)])

#species field
median(map.adap2[which(range.total85==1)])
hist(map.adap2[which(range.total85==1)])
median(map.plast2[which(range.total85==1)])
hist(map.plast2[which(range.total85==1)])




