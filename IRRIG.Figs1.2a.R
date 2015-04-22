##Figure 1 and 2a creation 

### NOTE: links to data must be updated

########################################################################################################
#Data analysis
library(data.table)
library(ggmap)
library(gplots)
library(plotrix)
library(ggplot2)
library(maps)
library(ncdf)
library(chron)
library(sp)
#library(data.table)
library(reshape2)
library(foreach)
library(plyr)
library(ggmap)
library(boot)
library(gplots)
library(SDMTools)
library(grid)
library(gridExtra)
library(plotrix)
library(grDevices)


#################################
#######Yellowstone RUNS Exp 2

#dif10.100A<-as.data.table(read.csv("/Users/travis/difPRECC10.110Jul.nc", header = TRUE, sep = ",", quote="\"", dec="."))

nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/difPRECC10.110Jul.nc")
	# get latitudes and longitudes for NORTH AMERICA
	lat <- get.var.ncdf(nc,"lat",verbose=F)
	lat<-round(lat[56:78], 2)
	nlat <- dim(lat)
	lon <- get.var.ncdf(nc,"lon")
	lon<-lon[93:121]
	nlon <- dim(lon)
	# get the data and attributes North AMERICA
#	precl.array <- get.var.ncdf(nc,"PRECL",start= c(93,56,1), count= c(29,23,1)) 
	precc.array <- get.var.ncdf(nc,"PRECC",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precc.array.long <- as.vector(precc.array)
	precc.mat <- matrix(precc.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) #nrow=nlon*nlat*65.25
	lonlat <-  expand.grid(lon,lat)#,1:12) #1948:2013),
#p1<-cbind(lonlat$Var1, lonlat$Var2, precc.mat)	


nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/difPRECL10.110Jul.nc")
	precl.array <- get.var.ncdf(nc,"PRECL",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precl.array.long <- as.vector(precl.array)
	precl.mat <- matrix(precl.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) 


nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/difPRECL10.110A.nc")
	precl.array <- get.var.ncdf(nc,"PRECL",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precl.array.long <- as.vector(precl.array)
	precl.mat.A <- matrix(precl.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) 
	
nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/difPRECc10.110A.nc")
	precc.array <- get.var.ncdf(nc,"PRECC",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precc.array.long <- as.vector(precc.array)
	precc.mat.A <- matrix(precc.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) 

p1<-cbind(lonlat$Var1, lonlat$Var2, precc.mat,  precl.mat,  precc.mat.A, precl.mat.A)	


P2<-as.data.table(p1)
setnames(P2,c("lon","lat","preccJ", "preclJ", "preccA", "preclA")) 


P2$latlon<-P2$lat*1000000+P2$lon



#P2$Pchg <- ((P2$preccJ+P2$preclJ+P2$preccA+P2$preclA)/2)*86400000*62 #change mm total rain change for july and august
P2$Pchg2 <- ((P2$preccJ+P2$preclJ)+(P2$preccA+P2$preclA)/2)*86400000*30 #change mm total rain change for july and august
P2$Pchg <- ((P2$preclJ+P2$preclA)/2)*86400000*30 #change mm total rain change for july and august

PRECLJUL1<- P2$Pchg #(P2$preclJ)*86400000*31 
PR1J<-P2$preclJ
PR1A<-P2$preclA
PT1J<-(P2$preclJ+P2$preccJ)/2
PT1A<-(P2$preclA+P2$preccA)/2
PRECCJUL1<- ((P2$preccJ+P2$preccA)/2)*86400000*62
PT1<-P2$Pchg2
PL1<-P2$Pchg
P2$lon <- P2$lon-360



#select lat lon with greatest PRECL change
#cd2<-subset(P2, latlon %in% c(40740285, 40740287.5, 42630285, 42630287.5 )) #42630282.5,

#us_states <- map_data("state")
#s<-(ggplot(aes(x=lon, y=lat, fill=Pchg), data=P2) + geom_tile())
#s+ scale_fill_gradient2(low = "pink", high = "green") +geom_polygon(data=us_states, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)




#################################
#######Yellowstone RUNS Exp 3


nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/ctrlPRECC8.nc")
	# get latitudes and longitudes for NORTH AMERICA
	lat <- get.var.ncdf(nc,"lat",verbose=F)
	lat<-round(lat[56:78], 2)
	nlat <- dim(lat)
	lon <- get.var.ncdf(nc,"lon")
	lon<-lon[93:121]
	nlon <- dim(lon)
	# get the data and attributes North AMERICA
#	precl.array <- get.var.ncdf(nc,"PRECL",start= c(93,56,1), count= c(29,23,1)) 
	precc.array <- get.var.ncdf(nc,"PRECC",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precc.array.long <- as.vector(precc.array)
	ctrl.aug.precc.mat <- matrix(precc.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) #nrow=nlon*nlat*65.25
	lonlat <-  expand.grid(lon,lat)#,1:12) #1948:2013),
#p1<-cbind(lonlat$Var1, lonlat$Var2, precc.mat)	


nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/ctrlPRECL8.nc")
	precl.array <- get.var.ncdf(nc,"PRECL",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precl.array.long <- as.vector(precl.array)
	ctrl.aug.precl.mat <- matrix(precl.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) 


nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/ctrlPRECC7.nc")
	precl.array <- get.var.ncdf(nc,"PRECC",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precl.array.long <- as.vector(precl.array)
	ctrl.jul.precc.mat.A <- matrix(precl.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) 
	
nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/ctrlPRECL7.nc")
	precc.array <- get.var.ncdf(nc,"PRECL",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precc.array.long <- as.vector(precc.array)
	ctrl.jul.precl.mat.A <- matrix(precc.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) 

nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/irrigPRECC7.nc")
	precc.array <- get.var.ncdf(nc,"PRECC",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precc.array.long <- as.vector(precc.array)
	irrig.jul.precc.mat.A <- matrix(precc.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) 

nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/irrigPRECL7.nc")
	precc.array <- get.var.ncdf(nc,"PRECL",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precc.array.long <- as.vector(precc.array)
	irrig.jul.precl.mat.A <- matrix(precc.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) 

nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/irrigPRECL8.nc")
	precc.array <- get.var.ncdf(nc,"PRECL",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precc.array.long <- as.vector(precc.array)
	irrig.aug.precl.mat.A <- matrix(precc.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) 

nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/irrigPRECC8.nc")
	precc.array <- get.var.ncdf(nc,"PRECC",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precc.array.long <- as.vector(precc.array)
	irrig.aug.precc.mat.A <- matrix(precc.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) 

 

p3<-cbind(lonlat$Var1, lonlat$Var2, ctrl.aug.precc.mat, ctrl.aug.precl.mat, ctrl.jul.precc.mat.A, ctrl.jul.precl.mat.A, irrig.jul.precc.mat.A, irrig.jul.precl.mat.A, irrig.aug.precl.mat.A, irrig.aug.precc.mat.A    )	

P3<-as.data.table(p3)
setnames(P3,c("lon","lat","CpreccA", "CpreclA", "CpreccJ", "CpreclJ", "IpreccJ", "IpreclJ", "IpreclA", "IpreccA")) 

P3$Pchg2 <- ((P3$IpreccA+P3$IpreclA+P3$IpreccJ+P3$IpreclJ)/2 - (P3$CpreccA+P3$CpreclA+P3$CpreccJ+P3$CpreclJ)/2)*86400000*30 #change mm total rain change for july and august
P3$Pchg <- ((P3$IpreclJ+ P3$IpreclA)-(P3$CpreclJ+ P3$CpreclA))*86400000*30 #change mm total rain change for july and august
P3$Pchgc <- ((P3$IpreccJ+ P3$IpreccA)/2 -(P3$CpreccJ+ P3$CpreccA)/2 )*86400000*31 #change mm total rain change for july and august
PRECLJUL2<- P3$Pchg #((P3$IpreclJ)-(P3$CpreclJ))*86400000*31 
PR2J<-(P3$IpreclJ)-(P3$CpreclJ)
PR2A<-(P3$IpreclA)-(P3$CpreclA)
PT2J<-((P3$IpreclJ)-(P3$CpreclJ)+ (P3$IpreccJ)-(P3$CpreccJ))/2
PT2A<-((P3$IpreclA)-(P3$CpreclA)+ (P3$IpreccA)-(P3$CpreccA))/2

PRECCJUL2<-((P3$IpreccJ+P3$IpreccA)-(P3$CpreccJ+P3$CpreccA))*86400000*31 
PT2<- P3$Pchg2
PL2<- P3$Pchg
P3$lon2 <- P3$lon -360
#P3<-subset(P3, lat<46 & lat>39 & lon2> -82 & lon2< -68 & Pchg>3)
#us_states <- map_data("state")
#s<-(ggplot(aes(x=lon2, y=lat, fill= Pchg), data=P3) + geom_tile())
#s+ scale_fill_gradient2(low = "pink", high = "green") +geom_polygon(data=us_states, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)


#P3<-subset(P3, lat<45 & lat>40 & lon2> -95)

#P23<-P3$Pchg2+Pchg2

mean(P3$CpreccA)


#################################
#######MONOCACY RUNS MEANS Exp 1

nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/ctrlPRECC8mon.nc")
	# get latitudes and longitudes for NORTH AMERICA
	lat <- get.var.ncdf(nc,"lat",verbose=F)
	lat<-round(lat[56:78], 2)
	nlat <- dim(lat)
	lon <- get.var.ncdf(nc,"lon")
	lon<-lon[93:121]
	nlon <- dim(lon)
	# get the data and attributes North AMERICA
#	precl.array <- get.var.ncdf(nc,"PRECL",start= c(93,56,1), count= c(29,23,1)) 
	precc.array <- get.var.ncdf(nc,"PRECC",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precc.array.long <- as.vector(precc.array)
	ctrl.aug.precc.mat <- matrix(precc.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) #nrow=nlon*nlat*65.25
	lonlat <-  expand.grid(lon,lat)#,1:12) #1948:2013),
#p1<-cbind(lonlat$Var1, lonlat$Var2, precc.mat)	


nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/ctrlPRECL8mon.nc")
	precl.array <- get.var.ncdf(nc,"PRECL",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precl.array.long <- as.vector(precl.array)
	ctrl.aug.precl.mat <- matrix(precl.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) 


nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/ctrlPRECC7mon.nc")
	precl.array <- get.var.ncdf(nc,"PRECC",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precl.array.long <- as.vector(precl.array)
	ctrl.jul.precc.mat.A <- matrix(precl.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) 
	
nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/ctrlPRECL7mon.nc")
	precc.array <- get.var.ncdf(nc,"PRECL",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precc.array.long <- as.vector(precc.array)
	ctrl.jul.precl.mat.A <- matrix(precc.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) 

nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/irrigPRECC7mon.nc")
	precc.array <- get.var.ncdf(nc,"PRECC",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precc.array.long <- as.vector(precc.array)
	irrig.jul.precc.mat.A <- matrix(precc.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) 

nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/irrigPRECL7mon.nc")
	precc.array <- get.var.ncdf(nc,"PRECL",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precc.array.long <- as.vector(precc.array)
	irrig.jul.precl.mat.A <- matrix(precc.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) 

nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/irrigPRECL8mon.nc")
	precc.array <- get.var.ncdf(nc,"PRECL",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precc.array.long <- as.vector(precc.array)
	irrig.aug.precl.mat.A <- matrix(precc.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) 

nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/irrigPRECC8mon.nc")
	precc.array <- get.var.ncdf(nc,"PRECC",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	precc.array.long <- as.vector(precc.array)
	irrig.aug.precc.mat.A <- matrix(precc.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) 

 

p4<-cbind(lonlat$Var1, lonlat$Var2, ctrl.aug.precc.mat, ctrl.aug.precl.mat, ctrl.jul.precc.mat.A, ctrl.jul.precl.mat.A, irrig.jul.precc.mat.A, irrig.jul.precl.mat.A, irrig.aug.precl.mat.A, irrig.aug.precc.mat.A    )	

P4<-as.data.table(p4)
setnames(P4,c("lon","lat","CpreccA", "CpreclA", "CpreccJ", "CpreclJ", "IpreccJ", "IpreclJ", "IpreclA", "IpreccA"))

P4$Pchg2 <- (((P4$IpreccA+P4$IpreclA+P4$IpreccJ+P4$IpreclJ)/2)-((P4$CpreccA+P4$CpreclA+P4$CpreccJ+P4$CpreclJ)/2))*86400000*30 #change mm total rain change for july and august
#P4$Pchg <- ((P4$IpreclJ+ P4$IpreclA)-(P4$CpreclJ+ P4$CpreclA))*86400000*62 #change mm total rain change for july and august
P4$Pchg <- ((P4$IpreclJ+ P4$IpreclA)-(P4$CpreclJ+P4$CpreclA))*86400000*30 #change mm total rain change for july and august
PRECLJUL3<-P4$Pchg #((P4$IpreclJ)-(P4$CpreclJ))*86400000*31 
PRECCJUL3<-((P4$IpreccJ+P4$IpreccA)-(P4$CpreccJ+P4$CpreccA))*86400000*62 
P4$PRECLJUL<-(PRECLJUL1+PRECLJUL2+PRECLJUL3)/3
PR3J<-(P4$IpreclJ)-(P4$CpreclJ)
PR3A<-(P4$IpreclA)-(P4$CpreclA)
PT3J<-((P4$IpreclJ)-(P4$CpreclJ)+ (P4$IpreccJ)-(P4$CpreccJ))/2
PT3A<-((P4$IpreclA)-(P4$CpreclA)+ (P4$IpreccA)-(P4$CpreccA))/2
PT3<-P4$Pchg2
PL3<-P4$Pchg

P4$PRECCJUL<-(PRECCJUL1+PRECCJUL2+PRECCJUL3)/3
P4$PRECLJULpct<- (P4$PRECLJUL/((P4$CpreclJ+P4$CpreclA)/2))*100
#P4$CpreclJ<- P4$CpreclJ*86400000*31 
P4$PT<-(PT1+PT2+PT3)/3
P4$PT1<-(PT1)
P4$PT2<-(PT2)
P4$PT3<-(PT3)
#TOTAL PRECL
P4$PL<-(PL1+PL2+PL3)/3
P4$PL1<-(PL1)
P4$PL2<-(PL2)
P4$PL3<-(PL3)
mean(P4$IpreclJ)

P4$lon2 <- P4$lon -360
P5<-subset(P4, lat<52 & lat>25 ) #& lon2< -68 & lon2> -130)
us_states <- map_data("state")
world <- map_data("world")

dev.new(width=8, height=4)


s<-(ggplot(aes(x=lon2, y=lat, fill=(PL)), data=P5) + geom_tile())
s+geom_polygon(data=us_states, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)+ scale_fill_gradientn(colours=(c("pink", "pink","pink","white","white","white","green", "green" )), space = "Lab", na.value = "grey50",  guide = "colourbar")+ geom_polygon(data=world, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0) + theme(axis.line = element_line( size = 0.35)) + theme(axis.ticks.margin = unit(0.06, "cm")) + theme(axis.ticks.length = unit(0.1, "cm")) +theme( panel.background = element_rect(fill=NA),  panel.grid.major = element_blank(),panel.grid.minor = element_blank(), plot.background = element_rect(fill=NA))    + theme(axis.text.y = element_text(angle = 90, hjust = 0.5, size=11, colour="black"))+ theme(axis.text.x = element_text(  size=11, colour="black")) +scale_y_continuous(name=(expression(paste("Latitude (", degree, "N)")))) +scale_x_continuous(name=(expression(paste("Longitude (", degree, "E)"))))+ coord_cartesian(xlim = c(-130, -63), ylim = c(22, 52)) 





#+ scale_fill_gradient2(low = "pink", high = "green") 
#use daily data to see how much July PRECL is per day and total to see what a 3mm means...



P4$PR1J<-PR1J*86400000*30
P4$PR1A<-PR1A*86400000*30
P4$PR2J<-PR2J*86400000*30
P4$PR2A<-PR2A*86400000*30
P4$PR3J<-PR3J*86400000*30
P4$PR3A<-PR3A*86400000*30

P4$PT1J<-PT1J*86400000*30
P4$PT1A<-PT1A*86400000*30
P4$PT2J<-PT2J*86400000*30
P4$PT2A<-PT2A*86400000*30
P4$PT3J<-PT3J*86400000*30
P4$PT3A<-PT3A*86400000*30


ttFunc<-function(x)
{
	tt1<- t.test(c(x$PR1J, x$PR1A, x$PR2J, x$PR2A, x$PR3J, x$PR3A )) #, paired=T)
	#tt1<- t.test(c(x$PT1J, x$PT1A, x$PT2J, x$PT2A, x$PT3J, x$PT3A )) #, paired=T)
	#ifelse(x$PRECLJUL>1, return(tt1$p.value), NA)
	return(tt1$p.value)
	}
	

#ppT<-aggregate(PRECLJUL ~lat+lon, data=P4, FUN=function(x) ttFunc(x))


P4p<-ddply(P4, c("lat", "lon"), ttFunc)
P4p$lon2 <- P4p$lon -360
setnames(P4p, "V1", "pval")

us_states <- map_data("state")
s<-(ggplot(aes(x=lon2, y=lat, fill= pval<0.1), data=P4p) + geom_tile())
s +geom_polygon(data=us_states, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0) #+ scale_fill_gradient2(low = "pink", high = "green")

#####################################################################
#Download IRRIGATION DATA



nc<-open.ncdf("/Users/travis/GitHub/IRRIG/CESM.summary.data/irrig.ctrl.78diff.nc")
	# get latitudes and longitudes for NORTH AMERICA
	lat <- get.var.ncdf(nc,"lat",verbose=F)
	lat<-round(lat[56:78], 2)
	nlat <- dim(lat)
	lon <- get.var.ncdf(nc,"lon")
	lon<-lon[93:121]
	nlon <- dim(lon)
	# get the data and attributes North AMERICA
	area.array <- get.var.ncdf(nc,"area",start= c(93,56), count= c(29,23)) 
	irrig.array <- get.var.ncdf(nc,"QIRRIG",start= c(93,56,1), count= c(29,23,1)) 
	close.ncdf(nc)
	area.array.long <- as.vector(area.array)
	irrig.array.long <- as.vector(irrig.array)
	area.mat <- matrix(area.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) #nrow=nlon*nlat*65.25
	irrig.mat <- matrix(irrig.array.long, nrow=nlon*nlat, ncol=1, byrow=FALSE) #nrow=nlon*nlat*65.25
	lonlat <-  expand.grid(lon,lat)#,1:12) #1948:2013),
#p1<-cbind(lonlat$Var1, lonlat$Var2, precc.mat)	

i1<-cbind(lonlat$Var1, lonlat$Var2, irrig.mat, area.mat)	

I1<-as.data.table(i1)
setnames(I1,c("lon","lat","irrig.mm.s", "area.km2")) 


I1$lon2 <- I1$lon -360
I1$irrig.JAm3<-(I1$irrig.mm.s*0.0864*60)*(I1$area.km2) #convert mm/s to km3 for July and August
I1$irrig.JAm3[I1$irrig.JAm3 ==0]<-NA
us_states <- map_data("state")
world <- map_data("world")
#s<-(ggplot(aes(x=lon2, y=lat, fill=irrig2), data=I1) + geom_point( aes(size=(irrig2), alpha=0.5)))
#s +geom_polygon(data=us_states, aes(x=long, y=lat, group=group), colour="black", fill=NA, lwd=0.25) + scale_fill_gradient2(low = "pink", high = "green")

IRRIG<-I1$irrig.JAm3

dev.new(width=7, height=4)

s<- ggplot(aes(x=lon2, y=lat, fill=(PT)), data=P4) + geom_tile() +geom_segment(x = -127, y = 40.6, xend = -71, yend = 40.6, col="#CD5C5C", linetype="dashed") + geom_point( aes(size=(IRRIG), alpha=0.5))

s+geom_polygon(data=us_states, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)+ geom_polygon(data=world, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)+scale_fill_gradient2(low = "pink", high = "green", guide = guide_colourbar(title = "PRECT")) +geom_text(x= -127, y=40.6, label= "A", col="#CD5C5C", vjust=1.25) +geom_text(x= -71, y=40.6, label= "Aʹ", col="#CD5C5C", vjust=1.25) + theme(axis.line = element_line( size = 0.35))+ theme(axis.line.x =  element_blank())  + theme(axis.ticks.margin = unit(0.06, "cm")) + theme(axis.ticks.length = unit(0.1, "cm")) +theme( panel.background = element_rect(fill=NA),  panel.grid.major = element_blank(),panel.grid.minor = element_blank(), plot.background = element_rect(fill=NA)) + theme(axis.ticks.x=element_blank()) +theme(axis.text.x = element_text(colour= NA))  + theme(axis.text.y = element_text(angle = 90, hjust = 0.5, size=11, colour="black"))+ scale_y_continuous(name=(expression(paste("Latitude (", degree, "N)")))) + scale_x_continuous(element_blank()) + coord_cartesian(xlim = c(-130, -63), ylim = c(22, 52)) 




