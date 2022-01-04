setwd("C:/Users/camil/Documents/Estudos/Ecologia, Evolu????o e Conserva????o da Biodiversidade/PIBIC I/micrurus lemniscatus carvalhoi")
library(adegenet)
library(spdep)
library(raster) 
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(reshape2) 
library(geosphere)
library(fuzzyjoin)
library(mapdata)
library(ggmap) 
library(maptools)


carvalhoi<-read.csv("carvalhoi.csv",h=T) 
suit.pres<-read.csv("suit.pres.csv",h=T)
names(suit.pres)<-c("idex","long","lat","suit") 
write.table(suit.pres,"suit.pres.txt")
suit.fut45<-read.csv("suit.fut45.csv",h=T)
names(suit.fut45)<-c("idex","long","lat","suit")
suit.fut85<-read.csv("suit.fut85.csv",h=T)
names(suit.fut85)<-c("idex","long","lat","suit")
temp.pres<-read.table("bio # CCSM_Modern(1950-1999).txt",h=T)
pres.bio8<-temp.pres[,c(2,3,11)]
temp.fut45<-read.table("bio #baseline_Modern(1950-1999)# CCSM_rcp45(2080-2100).txt",h=T)
fut45.bio8<-temp.fut45[,c(2,3,11)]
temp.fut85<-read.table("bio #baseline_Modern(1950-1999)# CCSM_rcp85(2080-2100).txt",h=T)
fut85.bio8<-temp.fut85[,c(2,3,11)]

mean(suit.fut85$suit)
mean(suit.fut45$suit)
mean(suit.pres$suit) 


carvalhoi_split =data.frame(carvalhoi, colsplit(carvalhoi$species.long.lat, pattern=";", names=paste0("sep", 1:3)))
carvalhoi_split <-colsplit(carvalhoi$species.long.lat, pattern=";", names=paste0("sep", 1:3))
names(carvalhoi_split)<-c("Carvalhoi","long","lat")   

id<-1:142
carvalhoi.data<-data.frame(id,carvalhoi_split)

#join carvalhoi.data and pres.bio8 by aproximate location 
carvalhoi.occur.1<-geo_left_join(carvalhoi.data,pres.bio8,by=c("long","lat"),max_dist=28,method="haversine",distance_col="")
names(carvalhoi.occur.1)<-c("id","carvalhoi.occur","long.x","lat.x","long.y","lat.y","bio8","dist","nothing")

#remove repeated locations leaving only closest ones   
carvalhoi_occur<-distinct(carvalhoi_order,id, .keep_all = TRUE) 

#cleaning table 
carvalhoi_occurence<-carvalhoi_occur[,c(1,2,5,6,7)]  
names(carvalhoi_occurence)<-c( "id","carvalhoi","long","lat","bio8") 

###########
#Present
carvalhoi_occur_suit<-merge(carvalhoi_occurence,suit.pres,by=c("long","lat"))
carvalhoi.suit.pres<-merge(pres.bio8,suit.pres)
suit.occur.pres<-carvalhoi.suit.pres[,5]
min(carvalhoi_occur_suit$suit) 
max(carvalhoi_occur_suit$suit)
temp.pres<-carvalhoi.suit.pres[,3]
coord<-carvalhoi.suit.pres[,c(1,2)] 
long<-carvalhoi.suit.pres[1]
lat<-carvalhoi.suit.pres[2]

threshold<-min(carvalhoi_occur_suit$suit)

#Future RCP4.5
carvalhoi.suit.45<-merge(fut45.bio8,suit.fut45,by=c("long","lat"))
suit.occur.45<-carvalhoi.suit.45[,5] 
temp.fut45<-carvalhoi.suit.45[,3]

#Future RCP8.5
carvalhoi.suit.85<-merge(fut85.bio8,suit.fut85,by=c("long","lat"))
suit.occur.85<-carvalhoi.suit.85[,5]
temp.fut85<-carvalhoi.suit.85[,3]


table.pres<-merge(suit.pres,pres.bio8,by=c("long","lat"))
occur.pres<-ifelse(suit.occur.pres>threshold,1,0)
occur.fut45<-ifelse(suit.occur.45>threshold,1,0)
occur.fut85<-ifelse(suit.occur.85>threshold,1,0) 


real.occur.pres<-ifelse(suit.occur.pres>threshold,suit.occur.pres,0)
real.occur.fut45<-ifelse(suit.occur.45>threshold,suit.occur.45,0)
real.occur.fut85<-ifelse(suit.occur.85>threshold,suit.occur.85,0) 

occur.obs.pres<-which(occur.pres==1)
occur.obs.fut45<-which(occur.fut45==1)
occur.obs.fut85<-which(occur.fut85==1)
length(occur.obs.pres)
length(occur.obs.fut45)
length(occur.obs.fut85)

#Future RCP 4.5 Regions
stable.region45<-ifelse(occur.pres==1 & occur.fut45==1,1,0)
leading.edge45<-ifelse((occur.pres-occur.fut45)< 0,1,0)
trailing.edge45<-ifelse(occur.pres==1 & occur.fut45==0,1,0)
trailing.edge45.1<-which(trailing.edge45==1)
length(trailing.edge45.1)
range.total45<-ifelse((occur.pres+occur.fut45) >= 1,1,0) 
field45<-which(range.total45==1)
field45.size<-length(field45) 

ms <-cbind(range.total45*temp.fut45, range.total45*temp.pres)
suit.rg2 <-numeric()
for(i in 1:length(temp.pres)){
  suit.rg2[i] <-max(ms[i,1],ms[i,2])
}
hist(suit.rg2)
suit0.field <-suit.rg2[field45]

#Future RCP 8.5 Regions
stable.region85<-ifelse(occur.pres==1 & occur.fut85==1,1,0)
leading.edge85<-ifelse((occur.pres-occur.fut85)< 0,1,0)
trailing.edge85<-ifelse(occur.pres==1 & occur.fut85==0,1,0)
trailing.edge85.1<-which(trailing.edge85==1)
length(trailing.edge85.1)
range.total85<-ifelse((occur.pres+occur.fut85) >= 1,1,0) 
field85<-which(range.total85==1)
field85.size<-length(field85) 

ms.85 <-cbind(range.total85*temp.fut85, range.total85*temp.pres)
suit.rg2 <-numeric()
for(i in 1:length(temp.pres)){
  suit.rg2[i] <-max(ms[i,1],ms[i,2])
}
hist(suit.rg2)
suit0.field.85 <-suit.rg2[field85]



###Temperature Analysis  
#Present
min(temp.pres[occur.obs.pres])
max(temp.pres[occur.obs.pres])
mean(temp.pres[occur.obs.pres])
sd(temp.pres[occur.obs.pres])
hist.temp.pres<-hist(temp.pres[occur.obs.pres],breaks=pretty(10:35,n=10),col="gray70",main="",xlim=c(10,35),xlab=" Current mean temperature of wettest quarter (worldclim Bio8)",ylab="Cells")
length(temp.pres[occur.obs.pres])

#Future RCP4.5
min(temp.fut45[occur.obs.fut45])
max(temp.fut45[occur.obs.fut45])
mean(temp.fut45[occur.obs.fut45])
hist.temp.fut45<-hist(temp.fut45[occur.obs.fut45],breaks=pretty(10:35,n=10),col="gray70",main="",xlim=c(10,35),xlab=" Current mean temperature of wettest quarter (worldclim Bio8)",ylab="Cells")
length(temp.fut45[occur.obs.fut45])

#Future RCP8.5
min(temp.fut85[occur.obs.fut85])
max(temp.fut85[occur.obs.fut85])
mean(temp.fut85[occur.obs.fut85])
hist.temp.fut85<-hist(temp.fut85[occur.obs.fut85],breaks=pretty(10:35,n=10),col="gray70",main="",xlim=c(10,35),xlab=" Current mean temperature of wettest quarter (worldclim Bio8)",ylab="Cells")

#Diference Pres/Fut45
plot(hist.temp.pres, col=rgb(0,0,0,1/2),
     main = "Present and RCP4.5" ,
     xlab = "Mean Temperature of Wettest Quarter(Bio8)",
     ylab = "Cells",
     ylim =c(0,1500),
     xlim=c(10,35))
plot(hist.temp.fut45, col=rgb(1,1,1,1/3), add=T)
abline(v=mean(temp.pres[occur.obs.pres]),col="green")
abline(v=mean(temp.fut45[occur.obs.fut45]),col="red")

#Diference Pres/Fut85
plot(hist.temp.pres, col=rgb(0,0,0,1/2),
     main = "Present and RCP8.5" ,
     xlab = "Mean Temperature of Wettest Quarter(Bio8)",
     ylab = "Cells",
     ylim =c(0,1500),
     xlim=c(10,35))
plot(hist.temp.fut85, col=rgb(1,1,1,1/4), add=T)
abline(v=mean(temp.pres[occur.obs.pres]),col="green")
abline(v=mean(temp.fut85[occur.obs.fut85]),col="red")
#######################################

area<-map_data("world", region = c("argentina", "bolivia", "brazil", "chile", "colombia", "ecuador", "guyana", "paraguay", "peru", "suriname", "uruguay", "venezuela"))
dist.pres<-data.frame(coord,occur.pres) 
dp<-subset(dist.pres,occur.pres== 1)
dist.pres1<-dp[c(1,2)] 

dist.f45<-data.frame(coord,occur.fut45)
df45<-subset(dist.f45,occur.fut45==1)
dist.f45.1<-df45[c(1,2)]

dist.f85<-data.frame(coord,occur.fut85)
df85<-subset(dist.f85,occur.fut85==1)
dist.f85.1<-df85[c(1,2)]

mapa_completo<-ggplot() + 
  geom_polygon(data = area, aes(x=long, y = lat,group=group), fill = NA, color = "antiquewhite4") + 
  coord_fixed(1.1) + geom_point(data = dist.pres1, aes(x = long, y = lat), 
                                color = "dimgray", #Escolha a cor dos pontos
                                size = 1, #Tamanho dos pontos
                                alpha = 1)+ geom_point(data = dist.f45.1, aes(x = long, y = lat), 
                                                       color = "darkgrey", #Escolha a cor dos pontos
                                                       size = 1, #Tamanho dos pontos
                                                       alpha = 1)+ #Transparencia: quanto mais proximo de 1, menos transparente
  geom_point(data = dist.f85.1, aes(x = long, y = lat), 
             color = "gainsboro", #Escolha a cor dos pontos
             size = 1, #Tamanho dos pontos
             alpha = 1)  #Transparencia: quanto mais proximo de 1, menos transparente



#Maps
map_pres <- rasterFromXYZ(xyz =  cbind(coord,occur.pres) , res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_pres, col = brewer.pal(n = 4, name = "Greys"))

map_fut45<-rasterFromXYZ(xyz =  cbind(coord,occur.fut45) , res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_fut45,col = brewer.pal(n = 4, name = "Greys")) 

map_fut85<-rasterFromXYZ(xyz =  cbind(coord,occur.fut85) , res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
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

map<-rasterFromXYZ(xyz =  cbind(coord,n2), res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map,col =  brewer.pal(n = 4, name = "PuBu"))

n<-rep(0,6159)
n1<-1
n2<-c(n,n1)


###Rescue Futuro RCP4.5
DeltaT45<-temp.fut45[which(trailing.edge45==1)]-temp.pres[which(trailing.edge45==1)]
mean(DeltaT45)

#Standarts deviations 
sd1<-sd(temp.pres[occur.obs.pres])
sd2<-sd(carvalhoi_occur_suit$bio8) 
sd3<-(max(temp.pres[occur.obs.pres])-min(temp.pres[occur.obs.pres]))/5  
((sd1+sd2)/2)^2



generations<-90/2.5


plast<-(29.9-15.2)/5  

w2<- ((sd1+sd2)/2)^2*0.3*50


rho <- 0.9 #autoregressive coefficient
neigh <-10 #neighbour size
longF <-carvalhoi.suit.45[field45,1]
latF <-carvalhoi.suit.45[field45,2]
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
  FST <-runif(field45.size,0.7,0.8) 
  sdT<-runif(field45.size,sd2,sd1) 
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
  FST <-(((0.8 - 0.7)*(FST.geo - min(FST.geo)))/ (max(FST.geo)-min(FST.geo))) + 0.7
  g45 <-round(((40 - 30)*(g.geo - min(g.geo)))/ (max(g.geo)-min(g.geo))) + 40
  bp <-(((0.15 - 0.1)*(bp.geo - min(bp.geo)))/ (max(bp.geo)-min(bp.geo))) + 0.1
  
  
  
  lambda <-(((1.12 - 1.04)*(suit0.field - min(suit0.field)))/ (max(suit0.field)-min(suit0.field))) + 1.04
  
  w2<- ((sd1+sd2)/2)^2*0.3*50
  
  #Model parameters	
  sdT1 <-sqrt((1 - FST) * (sdT0*sdT0))
  Vs <- w2 + ((sdT0*sdT0)-(h2*(sdT0*sdT0)))
  vT <-sdT1*sdT1
  vA <-vT*h2
  GT <-90/g45
  mean(GT)
  
  #across the map...
  
  Haldane45<-((temp.fut45[field45]-temp.pres[field45])/sdT1)/g45 
  
  #MSER
  HDC.45<-(vA*(sqrt(2*log(lambda*sqrt(w2/(vA+Vs)))/(vA+Vs))))/sqrt(vT) 
  
  
  #MSERp
  log(x = 10,base = exp(1)) 
  HDP.45 <-sqrt(((2*log(lambda)*(1/Vs))/GT)*((h2*(sdT*sdT))/(1 - bp)))
  
  
  HDC.45[is.na(HDC.45)] <- 0
  HDP.45[is.na(HDP.45)] <- 0
  
  
  #Adaptation 
  adapC.45<-(ifelse(Haldane45 < HDC.45,1,0)) 
  resultC.45<-which(adapC.45==1)
  tamanhoC.45<-length(resultC.45) 
  #length(adapC.45)
  #length(Haldane45)
  #length(HDC.45)
  
  adapP.45<-(ifelse(Haldane45 < HDP.45,1,0))
  resultP.45<-which(adapP.45==1)
  tamanhoP.45<-length(resultP.45)
  
  
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
  out[i,7] <-sum(adap.Neo*trailing.edge45)/sum(trailing.edge45) #adap  Trailing
  out[i,8] <-sum(adapP.Neo*trailing.edge45)/sum(trailing.edge45) #adapP  Trailing
  out[i,9] <-sum(adap.Neo*occur.fut45)/sum(occur.fut45)
  out[i,10] <-sum(adapP.Neo*occur.fut45)/sum(occur.fut45)
  
  
  #map of all species' field
  geo.adap[,i] <-adap.Neo 
  geo.adapP[,i] <-adapP.Neo
  geo.hald[,i] <-hald.Neo 
}


hist(out[,3],xlab="Mean Haldanes in trailing edge",ylab="Number of simulations",nclass=50,col="gray",main="" )
t.test(out[,4],out[,3]) 

mean.hald<-mean(out[,3]) 
mean(out[,3])
min(out[,3])
max(out[,3])
sd(out[,3])  
max(out[,1])

hist(out[,1],xlab="Mean MSER in trailing edge",ylab="Number of simulations",nclass=100,col="gray",main="")
mean(out[,1])
sd(out[,1])
abline(v=mean(out[,3]),col="black",lwd = 2)
abline(v=mean(out[,3])+sd(out[,3]),col="black",lwd = 1, lty = 2)
abline(v=mean(out[,3])-sd(out[,3]),col="black",lwd = 1, lty = 2)

hist(out[,2],xlab="Mean MSERp in trailing edge",ylab="Number of simulations",nclass=100,col="gray",main="")
mean(out[,2])
sd(out[,2])
abline(v=mean(out[,3]),col="black",lwd = 2)
abline(v=mean(out[,3])+sd(out[,3]),col="black",lwd = 1, lty = 2)
abline(v=mean(out[,3])-sd(out[,3]),col="black",lwd = 1, lty = 2)


hist(out[,7],xlab="Proportion of trailing edge rescued",ylab="Number of simulations",nclass=100,col="gray",main="")
hist(out[,8],xlab="Proportion of trailing edge rescued",ylab="Number of simulations",nclass=100,col="gray",main="")
median(out[,8])
max(out[,7])

hist(out[,5],xlab="Proportion of trailing edge rescued",ylab="Number of simulations",nclass=100,col="grey",main="",xlim=c(0,0.5),ylim=c(0,200))
hist(out[,9],xlab="Proportion of trailing edge rescued",ylab="Number of simulations",nclass=100,col="grey",main="",xlim=c(0,0.4),ylim=c(0,200))

#synthetizing among simulations...
sum(ifelse(out[,7] == 0.0,1,0))/nsim
sum(ifelse(out[,7] > 0.050,1,0))/nsim
sum(ifelse(out[,7] != 0.0,1,0))/nsim

sum(ifelse(out[,8] > 0.05,1,0))/nsim
sum(ifelse(out[,8] == 0.0,1,0))/nsim
sum(ifelse(out[,8] != 0.0,1,0))/nsim


#in the future region...
sum(ifelse(out[,9] > 0.050,1,0))/nsim
sum(ifelse(out[,9] == 0.0,1,0))/nsim
hist(out[,9])
max(out[,5])

#map analysis
map.adap <-apply(geo.adap,1,mean)
map.plast <-apply(geo.adapP,1,mean) 



#Map Adaptation  without plasticity RCP.4.5 (Trailing)
p.adap<-(apply(geo.adap,1,mean))*trailing.edge45 
map_adap <- rasterFromXYZ(xyz =  cbind(coord,p.adap) , res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_adap, col = brewer.pal(n = 4, name = "Greys")) 

###
plot(map_fut45, col=rgb(0,0,0,1/4), add=T)
plot(map_adap, col=rgb(1,1,1,1/4), add=T)

#
teste<-cbind(coord,p.adap,occur.fut45)
teste1<-teste$p.adap+teste$occur.fut45


#Map Adaptation with plasticity RCP4.5  (Trailing)
a45<-p.adap+occur.fut45
p.adapP<-(apply(geo.adapP,1,mean))*trailing.edge45 
map_adapP <- rasterFromXYZ(xyz =  cbind(coord,a45) , res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_adapP, col = brewer.pal(n = 4, name = "Greys")) 


a45m<-p.adapP+occur.fut45
map_adapP <- rasterFromXYZ(xyz =  cbind(coord,a45m) , res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_adapP, col = brewer.pal(n = 4, name = "Greys")) 

#trailing
median(map.adap[which(trailing.edge45==1)])
hist(map.adap[which(trailing.edge45==1)])
sum(ifelse(map.adap[which(trailing.edge45==1)]>0.05,1,0))/sum(trailing.edge45)
median(map.plast[which(trailing.edge45==1)])
hist(map.plast[which(trailing.edge45==1)])

#future range
median(map.adap[which(occur.fut45==1)])
hist(map.adap[which(occur.fut45==1)])
sum(ifelse(map.adap[which(occur.fut45==1)]>0.05,1,0))/sum(occur.fut45)

median(map.plast[which(occur.fut45==1)])
hist(map.plast[which(occur.fut45==1)])

#species field
median(map.adap[which(range.total45==1)])
hist(map.adap[which(range.total45==1)])
median(map.plast[which(range.total45==1)])
hist(map.plast[which(range.total45==1)])


#Rescue Analysis to RCP8.5
DeltaT85<-temp.fut85[which(trailing.edge85==1)]-temp.pres[which(trailing.edge85==1)]
mean(DeltaT85)

#Standard Deviation 
sd1<-sd(temp.pres[occur.obs.pres])
sd2<-sd(carvalhoi_occur_suit$bio8) 
sd3<-(max(temp.pres[occur.obs.pres])-min(temp.pres[occur.obs.pres]))/5  

g85<- runif(field85.size,35,45)

plast<-(29.9-15.2)/5 

rho <- 0.9 #autoregressive coefficient
neigh <-10 #neighbour size
longF.85 <-carvalhoi.suit.85[field85,1]
latF.85 <-carvalhoi.suit.85[field85,2]
coordsF.85 <-cbind(longF.85,latF.85) 
dnb.85 <- dnearneigh(coordsF.85, 0, neigh)
dsts.85 <- nbdists(dnb.85, coordsF.85)
idw.85 <- lapply(dsts.85, function(x) 1/x)
lw.85 <- nb2listw(dnb.85, glist=idw.85, style="W") 
inv.85<- invIrW(lw.85, rho)  #vector to multiply data...


#Comparing Haldanes
plot(hist.haldane85, col=rgb(0,0,0,1/2),
     main = "Histogramas",
     xlab = "Haldane",
     ylab = "Frequency",
     ylim =c(0,1500),
     xlim=c(0,0.10))
plot(hist.haldane85, col=rgb(1,1,1,1/4), add=T)
abline(v=mean(Haldane85),col="red")
abline(v=mean(Haldane85),col="red")


#Simulations...
nsim<-5000
geo.adap2 <-matrix(0,nrow(coord),nsim)
geo.adapP2 <-matrix(0,nrow(coord),nsim)
geo.hald2 <-matrix(0,nrow(coord),nsim)
out2 <-matrix(0,nsim,10)

for(i in 1:nsim){
  
  h2.85 <-runif(field85.size,0.20,0.40)
  FST.85 <-runif(field85.size,0.7,0.8) 
  sdT.85<-runif(field85.size,sd2,sd1) 
  g1.85<- runif(field85.size,30,40) 
  bp85<-runif(field85.size,0.00001,(plast^2/(mean(sdT))^2))  
  
  
  
  #Creating regional patterns in parameters
  h2.geo.85 <- inv.85%*%h2.85
  sdT.geo.85 <- inv.85%*%sdT.85
  FST.geo.85 <- inv.85%*%FST.85
  g.geo.85 <- inv.85%*%g1.85
  bp.geo.85 <- inv.85%*%bp85
  
  
  
  #Rescaling to previous ranges	
  h2.85 <-(((0.4 - 0.2)*(h2.geo.85 - min(h2.geo.85)))/ (max(h2.geo.85)-min(h2.geo.85))) + 0.2
  sdT0.85 <-(((sd2 - sd1)*(sdT.geo.85 - min(sdT.geo.85)))/ (max(sdT.geo.85)- min(sdT.geo.85))) + sd1
  FST.85 <-(((0.8 - 0.7)*(FST.geo.85 - min(FST.geo.85)))/ (max(FST.geo.85)-min(FST.geo.85))) + 0.7
  g85 <-round(((40 - 30)*(g.geo.85 - min(g.geo.85)))/ (max(g.geo.85)-min(g.geo.85))) + 40
  bp85 <-(((0.15 - 0.1)*(bp.geo.85 - min(bp.geo.85)))/ (max(bp.geo.85)-min(bp.geo.85))) + 0.1
  
  lambda.85 <-(((1.12 - 1.04)*(suit0.field.85 - min(suit0.field.85)))/ (max(suit0.field.85)-min(suit0.field.85))) + 1.09
  
  w2<- ((sd1+sd2)/2)^2*0.3*50
  
  #Model parameters	
  sdT1.85 <-sqrt((1 - FST.85) * (sdT0.85*sdT0.85))
  Vs.85 <- w2 + ((sdT0.85*sdT0.85)-(h2.85*(sdT0.85*sdT0.85)))
  vT.85<-sdT1.85*sdT1.85
  vA.85 <-vT.85*h2.85
  GT.85 <-90/g85
  
  Haldane85<-((temp.fut85[field85]-temp.pres[field85])/sdT1.85)/g85 
  
  #MSER
  HDC.85 <-(vA.85*(sqrt(2*log(lambda.85*sqrt(w2/(vA.85+Vs.85)))/(vA.85+Vs.85))))/sqrt(vT.85) 
  mean(HDC.85)
  max(HDC.85)
  min(HDC.85)
  hist(HDC.85,xlim=c(0.005,0.03),ylim=c(0,600))
  
  #MSERp
  log(x = 10,base = exp(1)) 
  HDP.85 <-sqrt(((2*log(lambda.85)*(1/Vs.85))/GT.85)*((h2.85*(sdT.85*sdT.85))/(1 - bp85)))
  mean(HDP.85)
  max(HDP.85)
  min(HDP.85)
  hist(HDP.85,xlim=c(0.03,0.08),ylim=c(0,600))
  
  
  HDC.85[is.na(HDC.85)] <- 0
  HDP.85[is.na(HDP.85)] <- 0
  
  
  #Adaptation
  adapC.85<-(ifelse(Haldane85 < HDC.85,1,0)) 
  resultC.85<-which(adapC.85==1)
  tamanhoC.85<-length(resultC.85) 
  
  
  adapP.85<-(ifelse(Haldane85 < HDP.85,1,0)) 
  resultP.85<-which(adapP.85==1)
  tamanhoP.85<-length(resultP.85)
  
  #Realinging the vector
  hald.Neo2 <-matrix(0,length(range.total85),1)
  for(j in 1:length(adapC.85)){
    hald.Neo2[field85[j]] <- Haldane85[j]
  }
  
  
  adap.Neo2 <-matrix(0,length(range.total85),1)
  for(j in 1:length(adapC.85)){
    adap.Neo2[field85[j]] <-adapC.85[j]
  }
  adapP.Neo2 <-matrix(0,length(range.total85),1)
  for(j in 1:length(adapP.85)){
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
  out2[i,5] <-sum(adap.Neo2*range.total85)/sum(range.total85) #Sem Plast 
  out2[i,6] <-sum(adapP.Neo2*range.total85)/sum(range.total85) #Com Plast
  out2[i,7] <-sum(adap.Neo2*trailing.edge85)/sum(trailing.edge85) #Sem Plast
  out2[i,8] <-sum(adapP.Neo2*trailing.edge85)/sum(trailing.edge85) #Com Plast
  out2[i,9] <-sum(adap.Neo2*occur.fut85)/sum(occur.fut85) #Sem Plast 
  out2[i,10] <-sum(adapP.Neo2*occur.fut85)/sum(occur.fut85) #Com Plast 
  
  
  #map of all species' field
  geo.adap2[,i] <-adap.Neo2 
  geo.adapP2[,i] <-adapP.Neo2
  geo.hald2[,i] <-hald.Neo2 
}


hist(out2[,3],xlab="Mean Haldanes in trailing edge RCP8.5",ylab="Number of simulations",nclass=50,col="gray",main="")
t.test(out2[,4],out2[,3])
mean(out2[,3])
sd(out2[,3])

hist(out2[,1],xlab="Mean MSER in trailing edge",ylab="Number of simulations",nclass=100,col="gray",main="")
mean(out2[,1])
sd(out2[,1])
abline(v=mean(out[,3]),col="black",lwd = 2)
abline(v=mean(out[,3])+sd(out[,3]),col="black",lwd = 1, lty = 2)
abline(v=mean(out[,3])-sd(out[,3]),col="black",lwd = 1, lty = 2)

hist(out2[,2],xlab="Mean MSERp in trailing edge",ylab="Number of simulations",nclass=100,col="gray",main="")
mean(out2[,2])
sd(out2[,2])
abline(v=mean(out[,3]),col="black",lwd = 2)
abline(v=mean(out[,3])+sd(out[,3]),col="black",lwd = 1, lty = 2)
abline(v=mean(out[,3])-sd(out[,3]),col="black",lwd = 1, lty = 2)


hist(out2[,7],xlab="Proportion of trailing edge rescued RCP8.5",ylab="Number of simulations",nclass=100,col="gray",main="")
hist(out2[,8],xlab="Proportion of trailing edge rescued RCP8.5",ylab="Number of simulations",nclass=100,col="gray",main="")
median(out2[,8])
max(out2[,7])

hist(out2[,5],xlab="Proportion of trailing edge rescued RCP8.5",ylab="Number of simulations",nclass=100,col="grey",main="")
hist(out2[,9],xlab="Proportion of trailing edge rescued RCP8.5",ylab="Number of simulations",nclass=100,col="grey",main="")

#synthetizing among simulations...
sum(ifelse(out2[,7] > 0.050,1,0))/nsim
sum(ifelse(out2[,7] != 0.00,1,0))/nsim

sum(ifelse(out2[,8] > 0.05,1,0))/nsim
sum(ifelse(out2[,8] != 0.0,1,0))/nsim

#in the future region...
sum(ifelse(out[,9] > 0.050,1,0))/nsim
sum(ifelse(out[,9] == 0.0,1,0))/nsim
hist(out[,9])
max(out[,5])

#map analysis
map.adap2 <-apply(geo.adap2,1,mean)
map.plast2 <-apply(geo.adapP2,1,mean)  



#Map Adaptation without plasticity RCP.8.5 (Trailing)
p.adap2<-(apply(geo.adap2,1,mean))*trailing.edge85 
a85<-p.adap2+occur.fut85
map_adap2 <- rasterFromXYZ(xyz =  cbind(coord,a85) , res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_adap2, col = brewer.pal(n = 4, name = "Greys")) 


#Map Adaptation with plasticity RCP8.5  (Trailing)
p.adapP2<-(apply(geo.adapP2,1,mean))*trailing.edge85 
a85p<-p.adapP2+occur.fut85
map_adapP2 <- rasterFromXYZ(xyz =  cbind(coord,a85p) , res = 0.5, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(map_adapP2, col = brewer.pal(n = 4, name = "Greys")) 

#trailing
median(map.adap[which(trailing.edge45==1)])
hist(map.adap[which(trailing.edge45==1)])
sum(ifelse(map.adap[which(trailing.edge45==1)]>0.05,1,0))/sum(trailing.edge45)
median(map.plast[which(trailing.edge45==1)])
hist(map.plast[which(trailing.edge45==1)])

#future range
median(map.adap[which(occur.fut45==1)])
hist(map.adap[which(occur.fut45==1)])
sum(ifelse(map.adap[which(occur.fut45==1)]>0.05,1,0))/sum(occur.fut45)

median(map.plast[which(occur.fut45==1)])
hist(map.plast[which(occur.fut45==1)])

#species field
median(map.adap[which(range.total45==1)])
hist(map.adap[which(range.total45==1)])
median(map.plast[which(range.total45==1)])
hist(map.plast[which(range.total45==1)])

