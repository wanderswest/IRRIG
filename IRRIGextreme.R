###IRRIG PAPER FINAL
#1/26/15


#FIGURE LIST
	#1.	Convective Precipitation with PRISM data
	#2.	CN changes map???
	#3.	PRECL map USA / with STORM TRACKS with system temps and where rain starts
	#4.	Extreme precipitation map US

###########################Figures 1 & 2 - USA Prect #####################









###########################Experiment 1 - Monocacy CARBON NITROGEN simulation #####################

module load netcdf-tools
cd /share/partition5/CESM/bfelzer/run/F2000CN-f19_g16/run

#copy 100 years of spunup files into one directory
#CAM - IRRIG July and August 
cp /home/share/partition4/CESM/archive/F2000CN_IRRIG-f19_g16/atm/hist/F2000CN_IRRIG-f19_g16.cam2.h0.17[0-7]*-07.nc eq7irrig
cp /home/share/partition4/CESM/archive/F2000CN_IRRIG-f19_g16/atm/hist/F2000CN_IRRIG-f19_g16.cam2.h0.16[8-9]*-07.nc eq7irrig
cp /home/share/partition4/CESM/archive/F2000CN_IRRIG-f19_g16/atm/hist/F2000CN_IRRIG-f19_g16.cam2.h0.17[0-7]*-08.nc eq8irrig
cp /home/share/partition4/CESM/archive/F2000CN_IRRIG-f19_g16/atm/hist/F2000CN_IRRIG-f19_g16.cam2.h0.16[8-9]*-08.nc eq8irrig

#CAM -CTRL July and August
cp /home/share/partition5/CESM/bfelzer/run/F2000CN-f19_g16/run/F2000CN-f19_g16.cam2.h0.17[0-7]*-07.nc eq7ctrl
cp /home/share/partition5/CESM/bfelzer/run/F2000CN-f19_g16/run/F2000CN-f19_g16.cam2.h0.16[8-9]*-07.nc eq7ctrl
cp /home/share/partition5/CESM/bfelzer/run/F2000CN-f19_g16/run/F2000CN-f19_g16.cam2.h0.17[0-7]*-08.nc eq8ctrl
cp /home/share/partition5/CESM/bfelzer/run/F2000CN-f19_g16/run/F2000CN-f19_g16.cam2.h0.16[8-9]*-08.nc eq8ctrl

#CLM - IRRIG July and August 
cp /home/share/partition4/CESM/archive/F2000CN_IRRIG-f19_g16/lnd/hist/F2000CN_IRRIG-f19_g16.clm2.h0.17[0-7]*-0[7-8].nc clmeq78irrig
cp /home/share/partition4/CESM/archive/F2000CN_IRRIG-f19_g16/lnd/hist/F2000CN_IRRIG-f19_g16.clm2.h0.16[8-9]*-0[7-8].nc clmeq78irrig


#CLM -CTRL July and August
cp /home/share/partition5/CESM/bfelzer/run/F2000CN-f19_g16/run/F2000CN-f19_g16.clm2.h0.17[0-7]*-0[7-8].nc clmeq78ctrl
cp /home/share/partition5/CESM/bfelzer/run/F2000CN-f19_g16/run/F2000CN-f19_g16.clm2.h0.16[8-9]*-0[7-8].nc clmeq78ctrl



#average precip
ncra -v PRECC F2000CN_IRRIG-f19_g16.cam2.h0*.nc ~/stats/irrigPRECC7mon.nc
ncra -v PRECL F2000CN_IRRIG-f19_g16.cam2.h0*.nc ~/stats/irrigPRECL7mon.nc
ncra F2000CN-f19_g16.clm2.h0*.nc ~/stats/ctrlCLM78.nc
ncra F2000CN_IRRIG-f19_g16.clm2.h0*.nc ~/stats/irrigCLM78.nc
ncdiff irrigCLM78.nc ctrlCLM78.nc irrig.ctrl.78diff.nc

#average all climate
ncra F2000CN_IRRIG-f19_g16.cam2.h0.17[0-7]*-0[7-8].nc F2000CN_IRRIG-f19_g16.cam2.h0.16[8-9]*-0[7-8].nc ~/stats/irrigCAM78.nc
ncra F2000CN-f19_g16.cam2.h0.17[0-7]*-0[7-8].nc F2000CN-f19_g16.cam2.h0.16[8-9]*-0[7-8].nc ~/stats/ctrlCAM78.nc
ncdiff irrigCAM78.nc ctrlCAM78.nc irrig.ctrl.CAM78diff.nc
scp tda210@monocacy.cc.lehigh.edu:/home/tda210/stats/*CAM78diff.nc .


###########################FIGURE 2 - CARBON NITROGEN DIFFERENCES#####################
#Average all CLM variables between contrl and irrigation
ncra F2000CN_IRRIG-f19_g16.clm2.h0*.nc ~/stats/irrigCLM78.nc
ncra F2000CN-f19_g16.clm2.h0*.nc ~/stats/ctrlCLM78.nc

ncdiff irrigCLM78.nc ctrlCLM78.nc irrig.ctrl.78diff.nc

#download files
scp tda210@monocacy.cc.lehigh.edu:/home/tda210/stats/*CLM78diff.nc .






###########################TABLE 1 and FIGURE 4-5 - Daily data extremes #####################


#########DAILY DATA########################################




#in R:
library(ncdf)
library(chron)
library(sp)
library(data.table)
library(reshape2)
library(foreach)
library(doParallel)
library(doMC)
registerDoMC(cores=4)


#get control run file names
IRRIGfiles<- NULL  #list.files("/glade/p/work/tandrews/F2_EXP3atm/spunupDAY78", pattern="*.nc", full.names=TRUE) #get file names
I8<-length(IRRIGfiles)

#get control run file names
CTRLfiles<-list.files("/glade/p/work/tandrews/F2_EXP3ctrlatm/spunupDAY78", pattern="*.nc", full.names=TRUE) #get file names
C8<-length(CTRLfiles)

allfiles<-c(IRRIGfiles, CTRLfiles)


P2<-foreach(f=1:length(allfiles), .combine=rbind) %do%
{	print(f)
	nc<-open.ncdf(allfiles[f])
	# get latitudes and longitudes for NORTH AMERICA
	lat <- get.var.ncdf(nc,"lat",verbose=F)
	lat<-round(lat[56:78], 2)
	nlat <- dim(lat)
	lon <- get.var.ncdf(nc,"lon")
	lon<-lon[93:121]
	nlon <- dim(lon)
	# get the data and attributes North AMERICA
	precl.array <- get.var.ncdf(nc,"PRECL",start= c(93,56,1), count= c(29,23,30)) 
	precc.array <- get.var.ncdf(nc,"PRECC",start= c(93,56,1), count= c(29,23,30)) 
	Z500.array <- get.var.ncdf(nc,"Z500",start= c(93,56,1), count= c(29,23,30)) 
	close.ncdf(nc)


	precl.array.long <- as.vector(precl.array)
	precl.mat <- matrix(precl.array.long, nrow=nlon*nlat*30, ncol=1, byrow=FALSE) #nrow=nlon*nlat*65.25
	precc.array.long <- as.vector(precc.array)
	precc.mat <- matrix(precc.array.long, nrow=nlon*nlat*30, ncol=1, byrow=FALSE) #nrow=nlon*nlat*65.25
	z500.array.long <- as.vector(Z500.array)
	z500.mat <- matrix(z500.array.long, nrow=nlon*nlat*30, ncol=1, byrow=FALSE) #nrow=nlon*nlat*65.25
	lonlat <-  expand.grid(lon,lat,1:30)#
	cond<-c(rep("irrig", I8), rep("ctrl", C8))
	month<-c(rep(f, length(lonlat$Var1)))
	condP2<-rep(cond[f], length(lonlat$Var1))
	p1 <-(cbind(lonlat$Var1, lonlat$Var2, lonlat$Var3, precl.mat, precc.mat, z500.mat, condP2, month))
}
print("done P2")
#write.csv(P2, "~/spunupF2EXP3/P2IRRIGdayPRECZ10.110.csv")
write.csv(P2, "~/spunupF2EXP3/P2CTRLdayPRECZ10.110.csv")
print("done and done")


####################################################################
#########Daily Analysis########

CTRL1 <-as.data.table(read.csv("~/Users/travis/GitHub/IRRIG/CESM.summary.data/P2CTRLdayPREC10.110.csv", header = TRUE, sep = ",", quote="\"", dec="."))
setnames(CTRL1, c("X2", "lon", "lat", "day", "PRECL", "PRECC", "cond", "month"))

CTRL1$latlon<-CTRL1$lat*1000000+CTRL1$lon

#select lat lon with greatest PRECL change
cd2<-subset(CTRL1,  lon>280 & lon<292 & lat>41 & lat<48 ) #latlon %in% c(40740285, 40740287.5, 42630285, 42630287.5 )) #42630282.5,

cd2$PRECC<-cd2$PRECC*86400000 #convert to mm/day
cd2$PRECL<-cd2$PRECL*86400000
cd2$PRECT<- cd2$PRECC+cd2$PRECL

#baseline control frequency for irrig comparison
cd2$pldays0.1<- (ave((cd2$PRECL<=quantile(cd2$PRECL, 0.5) & cd2$PRECL>0), cd2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) #divide by number of months for days/month
cd2$pldays0.5<- (ave(cd2$PRECL>quantile(cd2$PRECL, 0.5) & cd2$PRECL < quantile(cd2$PRECL, 0.99), cd2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) 
cd2$pldays0.99<- (ave(cd2$PRECL>quantile(cd2$PRECL, 0.99) & cd2$PRECL < quantile(cd2$PRECL, 0.995), cd2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30)
cd2$pldays0.995<- (ave(cd2$PRECL>quantile(cd2$PRECL, 0.995) & cd2$PRECL < quantile(cd2$PRECL, 0.999), cd2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30)  
cd2$pldays0.999<- (ave(cd2$PRECL>quantile(cd2$PRECL, 0.999), cd2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30)


cd2$plamt01<-ifelse(cd2$PRECL<=quantile(cd2$PRECL, 0.5) & cd2$PRECL>0, cd2$PRECL, NA)
cd2$plamt01ll<- (ave(cd2$plamt01, cd2$latlon, FUN=function(x) mean(x, na.rm=T))) 
cd2$plamt05<-ifelse(cd2$PRECL>=quantile(cd2$PRECL, 0.5) & cd2$PRECL<quantile(cd2$PRECL, 0.99), cd2$PRECL, NA)
cd2$plamt05ll<- (ave(cd2$plamt05, cd2$latlon, FUN=function(x) mean(x, na.rm=T))) 
cd2$plamt99<-ifelse(cd2$PRECL>=quantile(cd2$PRECL, 0.99) & cd2$PRECL<quantile(cd2$PRECL, 0.995), cd2$PRECL, NA)
cd2$plamt99ll<- (ave(cd2$plamt99, cd2$latlon, FUN=function(x) mean(x, na.rm=T))) 
cd2$plamt995<-ifelse(cd2$PRECL>=quantile(cd2$PRECL, 0.995), cd2$PRECL, NA)
cd2$plamt995ll<- (ave(cd2$plamt995, cd2$latlon, FUN=function(x) mean(x, na.rm=T))) 


#precC-

#baseline control frequency for irrig comparison
cd2$pcdays0.1<- (ave((cd2$PRECC<=quantile(cd2$PRECC, 0.5) & cd2$PRECC>0), cd2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) #divide by number of months for days/month
cd2$pcdays0.5<- (ave(cd2$PRECC>quantile(cd2$PRECC, 0.5) & cd2$PRECC < quantile(cd2$PRECC, 0.99), cd2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) 
cd2$pcdays0.99<- (ave(cd2$PRECC>quantile(cd2$PRECC, 0.99) & cd2$PRECC < quantile(cd2$PRECC, 0.995), cd2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30)
cd2$pcdays0.995<- (ave(cd2$PRECC>quantile(cd2$PRECC, 0.995) , cd2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30)  


cd2$pcamt01<-ifelse(cd2$PRECC<=quantile(cd2$PRECC, 0.5) & cd2$PRECC>0, cd2$PRECC, NA)
cd2$pcamt01ll<- (ave(cd2$pcamt01, cd2$latlon, FUN=function(x) mean(x, na.rm=T))) 
cd2$pcamt05<-ifelse(cd2$PRECC>=quantile(cd2$PRECC, 0.5) & cd2$PRECC<quantile(cd2$PRECC, 0.99), cd2$PRECC, NA)
cd2$pcamt05ll<- (ave(cd2$pcamt05, cd2$latlon, FUN=function(x) mean(x, na.rm=T))) 
cd2$pcamt99<-ifelse(cd2$PRECC>=quantile(cd2$PRECC, 0.99) & cd2$PRECC<quantile(cd2$PRECC, 0.995), cd2$PRECC, NA)
cd2$pcamt99ll<- (ave(cd2$pcamt99, cd2$latlon, FUN=function(x) mean(x, na.rm=T))) 
cd2$pcamt995<-ifelse(cd2$PRECC>=quantile(cd2$PRECC, 0.995), cd2$PRECC, NA)
cd2$pcamt995ll<- (ave(cd2$pcamt995, cd2$latlon, FUN=function(x) mean(x, na.rm=T))) 



##PRECT
cd2$ptdays0.1<- (ave(cd2$PRECT>0 & cd2$PRECC<=quantile(cd2$PRECC, 0.5), cd2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) #divide by number of months for days/month
cd2$ptdays0.5<- (ave(cd2$PRECT>=quantile(cd2$PRECT, 0.5) & cd2$PRECT<quantile(cd2$PRECT, 0.99), cd2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) 
cd2$ptdays0.99<- (ave(cd2$PRECT>=quantile(cd2$PRECT, 0.99)& cd2$PRECT<quantile(cd2$PRECT, 0.995), cd2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) 
cd2$ptdays0.995<- (ave(cd2$PRECT>=quantile(cd2$PRECT, 0.995), cd2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) 

cd2$ptamt01<-ifelse(cd2$PRECT<=quantile(cd2$PRECT, 0.5) & cd2$PRECT>0, cd2$PRECT, NA)
cd2$ptamt01ll<- (ave(cd2$ptamt01, cd2$latlon, FUN=function(x) mean(x, na.rm=T))) 
cd2$ptamt05<-ifelse(cd2$PRECT>=quantile(cd2$PRECT, 0.5) & cd2$PRECT<quantile(cd2$PRECT, 0.99), cd2$PRECT, NA)
cd2$ptamt05ll<- (ave(cd2$ptamt05, cd2$latlon, FUN=function(x) mean(x, na.rm=T))) 
cd2$ptamt99<-ifelse(cd2$PRECT>=quantile(cd2$PRECT, 0.99) & cd2$PRECT<quantile(cd2$PRECT, 0.995), cd2$PRECT, NA)
cd2$ptamt99ll<- (ave(cd2$ptamt99, cd2$latlon, FUN=function(x) mean(x, na.rm=T))) 
cd2$ptamt995<-ifelse(cd2$PRECT>=quantile(cd2$PRECT, 0.995), cd2$PRECT, NA)
cd2$ptamt995ll<- (ave(cd2$ptamt995, cd2$latlon, FUN=function(x) mean(x, na.rm=T))) 





CDmean<-subset(cd2, !duplicated(latlon))



(mean(id2days$ipldays0.1)- mean(cd2days$pldays0.1))/mean(cd2days$pldays0.1)*100)

RatioBoot <- sqrt(var(boot(id2, function(x,i){(sum(x$PRECL[i]>=quantile(cd2$PRECL, 0.99))-sum(cd2$PRECL>=quantile(cd2$PRECL, 0.99)))/sum(cd2$PRECL>=quantile(cd2$PRECL, 0.99))}, R=100)$t))			
RatioBoot <- boot(id2,y=0, z=0.5, function(x,y,z,i){(sum(x$PRECL[i]>=quantile(cd2$PRECL, y) & x$PRECL[i]< quantile(cd2$PRECL, z))-sum(cd2$PRECL>=quantile(cd2$PRECL, y) & cd2$PRECL< quantile(cd2$PRECL, z)))/sum(cd2$PRECL>=quantile(cd2$PRECL, y) & cd2$PRECL< quantile(cd2$PRECL, z))}, R=1000)				
mean(RatioBoot$t, na.rm=T)
error <-(sqrt(var(RatioBoot$t, na.rm=T))*1)
(sum(id2$PRECL>= 0 & id2$PRECL< quantile(cd2$PRECL, 0.5))-sum(cd2$PRECL>=0 & cd2$PRECL<quantile(cd2$PRECL, 0.5)))/sum(cd2$PRECL>=0 & cd2$PRECL<quantile(cd2$PRECL, 0.5))



####IRRIG file
IRRIG1 <-as.data.table(read.csv("~/Users/travis/GitHub/IRRIG/CESM.summary.data/P2IRRIGdayPREC10.110.csv", header = TRUE, sep = ",", quote="\"", dec="."))
setnames(IRRIG1, c("X2", "lon", "lat", "day", "PRECL", "PRECC", "cond", "month"))

IRRIG1$latlon<-IRRIG1$lat*1000000+IRRIG1$lon

#select lat lon with greatest PRECL change
id2<-subset(IRRIG1,  lon>280 & lon<292 & lat>41 & lat<48 ) #latlon %in% c(40740285, 40740287.5,  42630285, 42630287.5 )) #42630282.5,

id2$PRECC<-id2$PRECC*86400000 #convert to mm/day
id2$PRECL<-id2$PRECL*86400000
id2$PRECT<- id2$PRECC+id2$PRECL


#frequency PRECL
id2$ipldays0.1<- (ave(id2$PRECL>0 & id2$PRECL<=quantile(cd2$PRECL, 0.5), id2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) #divide by number of months for days/month
id2$ipldays0.5<- (ave(id2$PRECL>=quantile(cd2$PRECL, 0.5) & id2$PRECL<quantile(cd2$PRECL, 0.99), id2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) #use quantiles from control
id2$ipldays0.99<- (ave(id2$PRECL>=quantile(cd2$PRECL, 0.99) & id2$PRECL<quantile(cd2$PRECL, 0.995), id2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) #
id2$ipldays0.995<- (ave(id2$PRECL>=quantile(cd2$PRECL, 0.995), id2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) 

#amount PRECL
id2$plamt01<-ifelse(id2$PRECL<=quantile(id2$PRECL, 0.5) & id2$PRECL>0, id2$PRECL, NA)
id2$iplamt01ll<- (ave(id2$plamt01, id2$latlon, FUN=function(x) mean(x, na.rm=T))) 
id2$plamt05<-ifelse(id2$PRECL>=quantile(id2$PRECL, 0.5) & id2$PRECL<quantile(id2$PRECL, 0.99), id2$PRECL, NA)
id2$iplamt05ll<- (ave(id2$plamt05, id2$latlon, FUN=function(x) mean(x, na.rm=T))) 
id2$plamt99<-ifelse(id2$PRECL>=quantile(id2$PRECL, 0.99) & id2$PRECL<quantile(id2$PRECL, 0.995), id2$PRECL, NA)
id2$iplamt99ll<- (ave(id2$plamt99, id2$latlon, FUN=function(x) mean(x, na.rm=T))) 
id2$plamt995<-ifelse(id2$PRECL>=quantile(id2$PRECL, 0.995), id2$PRECL, NA)
id2$iplamt995ll<- (ave(id2$plamt995, id2$latlon, FUN=function(x) mean(x, na.rm=T))) 


###PRECC
#frequency 
id2$ipcdays0.1<- (ave(id2$PRECC>0 & id2$PRECC<=quantile(cd2$PRECC, 0.5), id2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) #divide by number of months for days/month
id2$ipcdays0.5<- (ave(id2$PRECC>=quantile(cd2$PRECC, 0.5) & id2$PRECC<quantile(cd2$PRECC, 0.99), id2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) #use quantiles from control
id2$ipcdays0.99<- (ave(id2$PRECC>=quantile(cd2$PRECC, 0.99) & id2$PRECC<quantile(cd2$PRECC, 0.995), id2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) #
id2$ipcdays0.995<- (ave(id2$PRECC>=quantile(cd2$PRECC, 0.995), id2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) 


#amount PRECC
id2$pcamt01<-ifelse(id2$PRECC<=quantile(id2$PRECC, 0.5) & id2$PRECC>0, id2$PRECC, NA)
id2$ipcamt01ll<- (ave(id2$pcamt01, id2$latlon, FUN=function(x) mean(x, na.rm=T))) 
id2$pcamt05<-ifelse(id2$PRECC>=quantile(id2$PRECC, 0.5) & id2$PRECC<quantile(id2$PRECC, 0.99), id2$PRECC, NA)
id2$ipcamt05ll<- (ave(id2$pcamt05, id2$latlon, FUN=function(x) mean(x, na.rm=T))) 
id2$pcamt99<-ifelse(id2$PRECC>=quantile(id2$PRECC, 0.99) & id2$PRECC<quantile(id2$PRECC, 0.995), id2$PRECC, NA)
id2$ipcamt99ll<- (ave(id2$pcamt99, id2$latlon, FUN=function(x) mean(x, na.rm=T))) 
id2$pcamt995<-ifelse(id2$PRECC>=quantile(id2$PRECC, 0.995), id2$PRECC, NA)
id2$ipcamt995ll<- (ave(id2$pcamt995, id2$latlon, FUN=function(x) mean(x, na.rm=T))) 



##total prec
#frequency PRECT
id2$iptdays0.1<- (ave(id2$PRECT>0 & id2$PRECT<=quantile(cd2$PRECT, 0.5), id2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) #divide by number of months for days/month
id2$iptdays0.5<- (ave(id2$PRECT>=quantile(cd2$PRECT, 0.5) & id2$PRECT<quantile(cd2$PRECT, 0.99), id2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) #use quantiles from control
id2$iptdays0.99<- (ave(id2$PRECT>=quantile(cd2$PRECT, 0.99) & id2$PRECT<quantile(cd2$PRECT, 0.995), id2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) #
id2$iptdays0.995<- (ave(id2$PRECT>=quantile(cd2$PRECT, 0.995), id2$latlon, FUN=function(x) sum(x, na.rm=T)))/(204*30) 


#amount average PRECT
id2$ptamt01<-ifelse(id2$PRECT<=quantile(id2$PRECT, 0.5) & id2$PRECT>0, id2$PRECT, NA)
id2$iptamt01ll<- (ave(id2$ptamt01, id2$latlon, FUN=function(x) mean(x, na.rm=T))) 
id2$ptamt05<-ifelse(id2$PRECT>=quantile(id2$PRECT, 0.5) & id2$PRECT<quantile(id2$PRECT, 0.99), id2$PRECT, NA)
id2$iptamt05ll<- (ave(id2$ptamt05, id2$latlon, FUN=function(x) mean(x, na.rm=T))) 
id2$ptamt99<-ifelse(id2$PRECT>=quantile(id2$PRECT, 0.99) & id2$PRECT<quantile(id2$PRECT, 0.995), id2$PRECT, NA)
id2$iptamt99ll<- (ave(id2$ptamt99, id2$latlon, FUN=function(x) mean(x, na.rm=T))) 
id2$ptamt995<-ifelse(id2$PRECT>=quantile(id2$PRECT, 0.995), id2$PRECT, NA)
id2$iptamt995ll<- (ave(id2$ptamt995, id2$latlon, FUN=function(x) mean(x, na.rm=T))) 


##new NA removed not paired

pl01tt<-t.test(id2$plamt01[is.na(id2$plamt01)==F],cd2$plamt01[is.na(cd2$plamt01)==F] )
pl01ttp<-c(mean(cd2$plamt01, na.rm=T), pl01tt$estimate[[1]]-pl01tt$estimate[[2]], pl01tt$p.value[[1]], pl01tt$conf.int[1], pl01tt$conf.int[2], mean((mean(id2$plamt01, na.rm=T)-mean(cd2$plamt01, na.rm=T))/(mean(cd2$plamt01, na.rm=T)))*100)
pl05tt<-t.test(id2$plamt05[is.na(id2$plamt05)==F],cd2$plamt05[is.na(cd2$plamt05)==F] )
pl05ttp<-c(mean(cd2$plamt05, na.rm=T), pl05tt$estimate[[1]]-pl05tt$estimate[[2]], pl05tt$p.value[[1]], pl05tt$conf.int[1], pl05tt$conf.int[2], mean((mean(id2$plamt05, na.rm=T)-mean(cd2$plamt05, na.rm=T))/(mean(cd2$plamt05, na.rm=T)))*100)
pl99tt<-t.test(id2$plamt99[is.na(id2$plamt99)==F],cd2$plamt99[is.na(cd2$plamt99)==F] )
pl99ttp<-c(mean(cd2$plamt99, na.rm=T), pl99tt$estimate[[1]]-pl99tt$estimate[[2]], pl99tt$p.value[[1]], pl99tt$conf.int[1], pl99tt$conf.int[2], mean((mean(id2$plamt99, na.rm=T)-mean(cd2$plamt99, na.rm=T))/(mean(cd2$plamt99, na.rm=T)))*100)
pl995tt<-t.test(id2$plamt995[is.na(id2$plamt995)==F],cd2$plamt995[is.na(cd2$plamt995)==F] )
pl995ttp<-c(mean(cd2$plamt995, na.rm=T), pl995tt$estimate[[1]]-pl995tt$estimate[[2]], pl995tt$p.value[[1]], pl995tt$conf.int[1], pl995tt$conf.int[2], mean((mean(id2$plamt995, na.rm=T)-mean(cd2$plamt995, na.rm=T))/(mean(cd2$plamt995, na.rm=T)))*100)



pc01tt<-t.test(id2$pcamt01[is.na(id2$pcamt01)==F],cd2$pcamt01[is.na(cd2$pcamt01)==F] )
pc01ttp<-c(mean(cd2$pcamt01, na.rm=T), pc01tt$estimate[[1]]-pc01tt$estimate[[2]], pc01tt$p.value[[1]], pc01tt$conf.int[1], pc01tt$conf.int[2], mean((mean(id2$pcamt01, na.rm=T)-mean(cd2$pcamt01, na.rm=T))/(mean(cd2$pcamt01, na.rm=T)))*100)
pc05tt<-t.test(id2$pcamt05[is.na(id2$pcamt05)==F],cd2$pcamt05[is.na(cd2$pcamt05)==F] )
pc05ttp<-c(mean(cd2$pcamt05, na.rm=T), pc05tt$estimate[[1]]-pc05tt$estimate[[2]], pc05tt$p.value[[1]], pc05tt$conf.int[1], pc05tt$conf.int[2], mean((mean(id2$pcamt05, na.rm=T)-mean(cd2$pcamt05, na.rm=T))/(mean(cd2$pcamt05, na.rm=T)))*100)
pc99tt<-t.test(id2$pcamt99[is.na(id2$pcamt99)==F],cd2$pcamt99[is.na(cd2$pcamt99)==F] )
pc99ttp<-c(mean(cd2$pcamt99, na.rm=T), pc99tt$estimate[[1]]-pc99tt$estimate[[2]], pc99tt$p.value[[1]], pc99tt$conf.int[1], pc99tt$conf.int[2], mean((mean(id2$pcamt99, na.rm=T)-mean(cd2$pcamt99, na.rm=T))/(mean(cd2$pcamt99, na.rm=T)))*100)
pc995tt<-t.test(id2$pcamt995[is.na(id2$pcamt995)==F],cd2$pcamt995[is.na(cd2$pcamt995)==F] )
pc995ttp<-c(mean(cd2$pcamt995, na.rm=T), pc995tt$estimate[[1]]-pc995tt$estimate[[2]], pc995tt$p.value[[1]], pc995tt$conf.int[1], pc995tt$conf.int[2], mean((mean(id2$pcamt995, na.rm=T)-mean(cd2$pcamt995, na.rm=T))/(mean(cd2$pcamt995, na.rm=T)))*100)


pt01tt<-t.test(id2$ptamt01[is.na(id2$ptamt01)==F],cd2$ptamt01[is.na(cd2$ptamt01)==F])
pt01ttp<-c(mean(cd2$ptamt01, na.rm=T), pt01tt$estimate[[1]]-pt01tt$estimate[[2]], pt01tt$p.value[[1]], pt01tt$conf.int[1], pt01tt$conf.int[2], mean((mean(id2$ptamt01, na.rm=T)-mean(cd2$ptamt01, na.rm=T))/(mean(cd2$ptamt01, na.rm=T)))*100)
pt05tt<-t.test(id2$ptamt05[is.na(id2$ptamt05)==F],cd2$ptamt05[is.na(cd2$ptamt05)==F] )
pt05ttp<-c(mean(cd2$ptamt05, na.rm=T), pt05tt$estimate[[1]]-pt05tt$estimate[[2]], pt05tt$p.value[[1]], pt05tt$conf.int[1], pt05tt$conf.int[2], mean((mean(id2$ptamt05, na.rm=T)-mean(cd2$ptamt05, na.rm=T))/(mean(cd2$ptamt05, na.rm=T)))*100)
pt99tt<-t.test(id2$ptamt99[is.na(id2$ptamt99)==F],cd2$ptamt99[is.na(cd2$ptamt99)==F] )
pt99ttp<-c(mean(cd2$ptamt99, na.rm=T), pt99tt$estimate[[1]]-pt99tt$estimate[[2]], pt99tt$p.value[[1]], pt99tt$conf.int[1], pt99tt$conf.int[2], mean((mean(id2$ptamt99, na.rm=T)-mean(cd2$ptamt99, na.rm=T))/(mean(cd2$ptamt99, na.rm=T)))*100)
pt995tt<-t.test(id2$ptamt995[is.na(id2$ptamt995)==F],cd2$ptamt995[is.na(cd2$ptamt995)==F] )
pt995ttp<-c(mean(cd2$ptamt995, na.rm=T), pt995tt$estimate[[1]]-pt995tt$estimate[[2]], pt995tt$p.value[[1]], pt995tt$conf.int[1], pt995tt$conf.int[2], mean((mean(id2$ptamt995, na.rm=T)-mean(cd2$ptamt995, na.rm=T))/(mean(cd2$ptamt995, na.rm=T)))*100)

tt.table<-rbind( pl01ttp, pl05ttp, pl99ttp, pl995ttp, pc01ttp, pc05ttp, pc99ttp, pc995ttp, pt01ttp, pt05ttp, pt99ttp, pt995ttp )

tt.table[,3]<-ifelse(tt.table[,3]<0.05, "***", "")
colnames(tt.table) <- c("CTRL amt", "IRRIG diff.", "sig.", "lower CI", "upper CI", "percent diff." )
write.csv(tt.table)



 id2days<-subset(id2, !duplicated(latlon))
 cd2days<-subset(cd2, !duplicated(latlon))

 pldays0.1<- c( mean(cd2days$pldays0.1), mean(id2days$ipldays0.1-cd2days$pldays0.1),  ((mean(id2days$ipldays0.1)- mean(cd2days$pldays0.1))/mean(cd2days$pldays0.1)*100), t.test(id2days$ipldays0.1, cd2days$pldays0.1, paired=T)$p.value[[1]] ) 
 pldays0.5<- c( mean(cd2days$pldays0.5), mean(id2days$ipldays0.5-cd2days$pldays0.5),  ((mean(id2days$ipldays0.5)- mean(cd2days$pldays0.5))/mean(cd2days$pldays0.5)*100), t.test(id2days$ipldays0.5, cd2days$pldays0.5, paired=T)$p.value[[1]] ) 
 pldays0.99<- c( mean(cd2days$pldays0.99), mean(id2days$ipldays0.99-cd2days$pldays0.99),  ((mean(id2days$ipldays0.99)- mean(cd2days$pldays0.99))/mean(cd2days$pldays0.99)*100), t.test(id2days$ipldays0.99, cd2days$pldays0.99, paired=T)$p.value[[1]] ) 
 pldays0.995<- c( mean(cd2days$pldays0.995), mean(id2days$ipldays0.995-cd2days$pldays0.995),  ((mean(id2days$ipldays0.995)- mean(cd2days$pldays0.995))/mean(cd2days$pldays0.995)*100), t.test(id2days$ipldays0.995, cd2days$pldays0.995, paired=T)$p.value[[1]] ) 

 pcdays0.1<- c( mean(cd2days$pcdays0.1), mean(id2days$ipcdays0.1-cd2days$pcdays0.1),  ((mean(id2days$ipcdays0.1)- mean(cd2days$pcdays0.1))/mean(cd2days$pcdays0.1)*100), t.test(id2days$ipcdays0.1, cd2days$pcdays0.1, paired=T)$p.value[[1]] ) 
 pcdays0.5<- c( mean(cd2days$pcdays0.5), mean(id2days$ipcdays0.5-cd2days$pcdays0.5),   ((mean(id2days$ipcdays0.5)- mean(cd2days$pcdays0.5))/mean(cd2days$pcdays0.5)*100), t.test(id2days$ipcdays0.5, cd2days$pcdays0.5, paired=T)$p.value[[1]] ) 
 pcdays0.99<- c( mean(cd2days$pcdays0.99), mean(id2days$ipcdays0.99-cd2days$pcdays0.99),  ((mean(id2days$ipcdays0.99)- mean(cd2days$pcdays0.99))/mean(cd2days$pcdays0.99)*100), t.test(id2days$ipcdays0.99, cd2days$pcdays0.99, paired=T)$p.value[[1]] ) 
 pcdays0.995<- c( mean(cd2days$pcdays0.995), mean(id2days$ipcdays0.995-cd2days$pcdays0.995),   ((mean(id2days$ipcdays0.995)- mean(cd2days$pcdays0.995))/mean(cd2days$pcdays0.995)*100), t.test(id2days$ipcdays0.995, cd2days$pcdays0.995, paired=T)$p.value[[1]] ) 
 
 ptdays0.1<- c( mean(cd2days$ptdays0.1), mean(id2days$iptdays0.1-cd2days$ptdays0.1), ((mean(id2days$iptdays0.1)- mean(cd2days$ptdays0.1))/mean(cd2days$ptdays0.1)*100), t.test(id2days$iptdays0.1, cd2days$ptdays0.1, paired=T)$p.value[[1]] ) 
 ptdays0.5<- c( mean(cd2days$ptdays0.5), mean(id2days$iptdays0.5-cd2days$ptdays0.5),  ((mean(id2days$iptdays0.5)- mean(cd2days$ptdays0.5))/mean(cd2days$ptdays0.5)*100), t.test(id2days$iptdays0.5, cd2days$ptdays0.5, paired=T)$p.value[[1]] ) 
 ptdays0.99<- c( mean(cd2days$ptdays0.99), mean(id2days$iptdays0.99-cd2days$ptdays0.99),  ((mean(id2days$iptdays0.99)- mean(cd2days$ptdays0.99))/mean(cd2days$ptdays0.99)*100), t.test(id2days$iptdays0.99, cd2days$ptdays0.99, paired=T)$p.value[[1]] ) 
 ptdays0.995<- c( mean(cd2days$ptdays0.995), mean(id2days$iptdays0.995-cd2days$ptdays0.995),  ((mean(id2days$iptdays0.995)- mean(cd2days$ptdays0.995))/mean(cd2days$ptdays0.995)*100), t.test(id2days$iptdays0.995, cd2days$ptdays0.995, paired=T)$p.value[[1]] ) 
 
 
freq.table<-rbind(pldays0.1, pldays0.5, pldays0.99, pldays0.995, pcdays0.1, pcdays0.5, pcdays0.99, pcdays0.995, ptdays0.1, ptdays0.5, ptdays0.99, ptdays0.995)
freq.table[,4]<-ifelse(freq.table[,4]<0.05, "***", "")
colnames(freq.table) <- c("CTRL freq", "IRRIG diff.","percent diff.", "sig.")
write.csv(freq.table)


###FREQUENCY TABLE BOOTSTRAPPED!

#PRECL
RatioBootPL0.1 <- boot(id2,y=0, z=0.5, function(x,y,z,i){(sum(x$PRECL[i]>=quantile(cd2$PRECL, y) & x$PRECL[i]< quantile(cd2$PRECL, z))-sum(cd2$PRECL>=quantile(cd2$PRECL, y) & cd2$PRECL< quantile(cd2$PRECL, z)))/sum(cd2$PRECL>=quantile(cd2$PRECL, y) & cd2$PRECL< quantile(cd2$PRECL, z))}, R=1000)				
pldays0.1<-c( sum(cd2$PRECL>=quantile(cd2$PRECL, 0) & cd2$PRECL< quantile(cd2$PRECL, 0.5))/sum(cd2$PRECL>=quantile(cd2$PRECL, 0)), mean(RatioBootPL0.1$t, na.rm=T), sqrt(var(RatioBootPL0.1$t, na.rm=T))*1)
RatioBootPL0.5 <- boot(id2,y=0.5, z=0.99, function(x,y,z,i){(sum(x$PRECL[i]>=quantile(cd2$PRECL, y) & x$PRECL[i]< quantile(cd2$PRECL, z))-sum(cd2$PRECL>=quantile(cd2$PRECL, y) & cd2$PRECL< quantile(cd2$PRECL, z)))/sum(cd2$PRECL>=quantile(cd2$PRECL, y) & cd2$PRECL< quantile(cd2$PRECL, z))}, R=1000)				
pldays0.5<-c( sum(cd2$PRECL>=quantile(cd2$PRECL, 0.5) & cd2$PRECL< quantile(cd2$PRECL, 0.99))/sum(cd2$PRECL>=quantile(cd2$PRECL, 0)), mean(RatioBootPL0.5$t, na.rm=T), sqrt(var(RatioBootPL0.5$t, na.rm=T))*1)
RatioBootPL0.99 <- boot(id2,y=0.99, z=0.995, function(x,y,z,i){(sum(x$PRECL[i]>=quantile(cd2$PRECL, y) & x$PRECL[i]< quantile(cd2$PRECL, z))-sum(cd2$PRECL>=quantile(cd2$PRECL, y) & cd2$PRECL< quantile(cd2$PRECL, z)))/sum(cd2$PRECL>=quantile(cd2$PRECL, y) & cd2$PRECL< quantile(cd2$PRECL, z))}, R=1000)				
pldays0.99<-c( sum(cd2$PRECL>=quantile(cd2$PRECL, 0.99) & cd2$PRECL< quantile(cd2$PRECL, 0.995))/sum(cd2$PRECL>=quantile(cd2$PRECL, 0)), mean(RatioBootPL0.99$t, na.rm=T), sqrt(var(RatioBootPL0.99$t, na.rm=T))*1)
RatioBootPL0.995 <- boot(id2,y=0.995, z=1, function(x,y,z,i){(sum(x$PRECL[i]>=quantile(cd2$PRECL, y) & x$PRECL[i]<= quantile(cd2$PRECL, z))-sum(cd2$PRECL>=quantile(cd2$PRECL, y) & cd2$PRECL<= quantile(cd2$PRECL, z)))/sum(cd2$PRECL>=quantile(cd2$PRECL, y) & cd2$PRECL<= quantile(cd2$PRECL, z))}, R=1000)				
pldays0.995<-c( sum(cd2$PRECL>=quantile(cd2$PRECL, 0.995) & cd2$PRECL<= quantile(cd2$PRECL, 1))/sum(cd2$PRECL>=quantile(cd2$PRECL, 0)), mean(RatioBootPL0.995$t, na.rm=T), sqrt(var(RatioBootPL0.995$t, na.rm=T))*1)


#PRECC
RatioBootpc0.1 <- boot(id2,y=0, z=0.5, function(x,y,z,i){(sum(x$PRECC[i]>=quantile(cd2$PRECC, y) & x$PRECC[i]< quantile(cd2$PRECC, z))-sum(cd2$PRECC>=quantile(cd2$PRECC, y) & cd2$PRECC< quantile(cd2$PRECC, z)))/sum(cd2$PRECC>=quantile(cd2$PRECC, y) & cd2$PRECC< quantile(cd2$PRECC, z))}, R=1000)				
pcdays0.1<-c( sum(cd2$PRECC>=quantile(cd2$PRECC, 0) & cd2$PRECC< quantile(cd2$PRECC, 0.5))/sum(cd2$PRECC>=quantile(cd2$PRECC, 0)), mean(RatioBootpc0.1$t, na.rm=T), sqrt(var(RatioBootpc0.1$t, na.rm=T))*1)
RatioBootpc0.5 <- boot(id2,y=0.5, z=0.99, function(x,y,z,i){(sum(x$PRECC[i]>=quantile(cd2$PRECC, y) & x$PRECC[i]< quantile(cd2$PRECC, z))-sum(cd2$PRECC>=quantile(cd2$PRECC, y) & cd2$PRECC< quantile(cd2$PRECC, z)))/sum(cd2$PRECC>=quantile(cd2$PRECC, y) & cd2$PRECC< quantile(cd2$PRECC, z))}, R=1000)				
pcdays0.5<-c( sum(cd2$PRECC>=quantile(cd2$PRECC, 0.5) & cd2$PRECC< quantile(cd2$PRECC, 0.99))/sum(cd2$PRECC>=quantile(cd2$PRECC, 0)), mean(RatioBootpc0.5$t, na.rm=T), sqrt(var(RatioBootpc0.5$t, na.rm=T))*1)
RatioBootpc0.99 <- boot(id2,y=0.99, z=0.995, function(x,y,z,i){(sum(x$PRECC[i]>=quantile(cd2$PRECC, y) & x$PRECC[i]< quantile(cd2$PRECC, z))-sum(cd2$PRECC>=quantile(cd2$PRECC, y) & cd2$PRECC< quantile(cd2$PRECC, z)))/sum(cd2$PRECC>=quantile(cd2$PRECC, y) & cd2$PRECC< quantile(cd2$PRECC, z))}, R=1000)				
pcdays0.99<-c( sum(cd2$PRECC>=quantile(cd2$PRECC, 0.99) & cd2$PRECC< quantile(cd2$PRECC, 0.995))/sum(cd2$PRECC>=quantile(cd2$PRECC, 0)), mean(RatioBootpc0.99$t, na.rm=T), sqrt(var(RatioBootpc0.99$t, na.rm=T))*1)
RatioBootpc0.995 <- boot(id2,y=0.995, z=1, function(x,y,z,i){(sum(x$PRECC[i]>=quantile(cd2$PRECC, y) & x$PRECC[i]<= quantile(cd2$PRECC, z))-sum(cd2$PRECC>=quantile(cd2$PRECC, y) & cd2$PRECC<= quantile(cd2$PRECC, z)))/sum(cd2$PRECC>=quantile(cd2$PRECC, y) & cd2$PRECC<= quantile(cd2$PRECC, z))}, R=1000)				
pcdays0.995<-c( sum(cd2$PRECC>=quantile(cd2$PRECC, 0.995) & cd2$PRECC<= quantile(cd2$PRECC, 1))/sum(cd2$PRECC>=quantile(cd2$PRECC, 0)), mean(RatioBootpc0.995$t, na.rm=T), sqrt(var(RatioBootpc0.995$t, na.rm=T))*1)

#PRECT
RatioBootpt0.1 <- boot(id2,y=0, z=0.5, function(x,y,z,i){(sum(x$PRECT[i]>=quantile(cd2$PRECT, y) & x$PRECT[i]< quantile(cd2$PRECT, z))-sum(cd2$PRECT>=quantile(cd2$PRECT, y) & cd2$PRECT< quantile(cd2$PRECT, z)))/sum(cd2$PRECT>=quantile(cd2$PRECT, y) & cd2$PRECT< quantile(cd2$PRECT, z))}, R=1000)				
ptdays0.1<-c( sum(cd2$PRECT>=quantile(cd2$PRECT, 0) & cd2$PRECT< quantile(cd2$PRECT, 0.5))/sum(cd2$PRECT>=quantile(cd2$PRECT, 0)), mean(RatioBootpt0.1$t, na.rm=T), sqrt(var(RatioBootpt0.1$t, na.rm=T))*1)
RatioBootpt0.5 <- boot(id2,y=0.5, z=0.99, function(x,y,z,i){(sum(x$PRECT[i]>=quantile(cd2$PRECT, y) & x$PRECT[i]< quantile(cd2$PRECT, z))-sum(cd2$PRECT>=quantile(cd2$PRECT, y) & cd2$PRECT< quantile(cd2$PRECT, z)))/sum(cd2$PRECT>=quantile(cd2$PRECT, y) & cd2$PRECT< quantile(cd2$PRECT, z))}, R=1000)				
ptdays0.5<-c( sum(cd2$PRECT>=quantile(cd2$PRECT, 0.5) & cd2$PRECT< quantile(cd2$PRECT, 0.99))/sum(cd2$PRECT>=quantile(cd2$PRECT, 0)), mean(RatioBootpt0.5$t, na.rm=T), sqrt(var(RatioBootpt0.5$t, na.rm=T))*1)
RatioBootpt0.99 <- boot(id2,y=0.99, z=0.995, function(x,y,z,i){(sum(x$PRECT[i]>=quantile(cd2$PRECT, y) & x$PRECT[i]< quantile(cd2$PRECT, z))-sum(cd2$PRECT>=quantile(cd2$PRECT, y) & cd2$PRECT< quantile(cd2$PRECT, z)))/sum(cd2$PRECT>=quantile(cd2$PRECT, y) & cd2$PRECT< quantile(cd2$PRECT, z))}, R=1000)				
ptdays0.99<-c( sum(cd2$PRECT>=quantile(cd2$PRECT, 0.99) & cd2$PRECT< quantile(cd2$PRECT, 0.995))/sum(cd2$PRECT>=quantile(cd2$PRECT, 0)), mean(RatioBootpt0.99$t, na.rm=T), sqrt(var(RatioBootpt0.99$t, na.rm=T))*1)
RatioBootpt0.995 <- boot(id2,y=0.995, z=1, function(x,y,z,i){(sum(x$PRECT[i]>=quantile(cd2$PRECT, y) & x$PRECT[i]<= quantile(cd2$PRECT, z))-sum(cd2$PRECT>=quantile(cd2$PRECT, y) & cd2$PRECT<= quantile(cd2$PRECT, z)))/sum(cd2$PRECT>=quantile(cd2$PRECT, y) & cd2$PRECT<= quantile(cd2$PRECT, z))}, R=1000)				
ptdays0.995<-c( sum(cd2$PRECT>=quantile(cd2$PRECT, 0.995) & cd2$PRECT<= quantile(cd2$PRECT, 1))/sum(cd2$PRECT>=quantile(cd2$PRECT, 0)), mean(RatioBootpt0.995$t, na.rm=T), sqrt(var(RatioBootpt0.995$t, na.rm=T))*1)

freq.table<-rbind(pldays0.1, pldays0.5, pldays0.99, pldays0.995, pcdays0.1, pcdays0.5, pcdays0.99, pcdays0.995, ptdays0.1, ptdays0.5, ptdays0.99, ptdays0.995)
freq.table[,3]<-ifelse(abs(freq.table[,2])>=freq.table[,3]* 1.96, "***", "")
colnames(freq.table) <- c("CTRL freq","percent diff.", "sig.")
write.csv(freq.table)


##########US 500 year stomr amout map
CTRL1b<-CTRL1  #subset(CTRL1, latlon %in% c(40740285, 40740287.5,  42630285, 42630287.5 ))
IRRIG1b<-IRRIG1 #subset(IRRIG1 , latlon %in% c(40740285, 40740287.5,  42630285, 42630287.5 ))
CTRL1b$PRECT<- (CTRL1b$PRECC + CTRL1b$PRECL)* 86400000
IRRIG1b$PRECT<- (IRRIG1b$PRECC + IRRIG1b$PRECL)* 86400000

#500 year storms
CTRL1b$ptamt995min<-(ave(CTRL1b$PRECT, CTRL1b$latlon, FUN=function(x) quantile(x, 0.995, na.rm=T))) 
CTRL1b$ptamt995<-ifelse(CTRL1b$PRECT>= CTRL1b$ptamt995min, CTRL1b$PRECT, NA) ###DO FOR EACH LAT LON!!!!!
CTRL1b$ptamt995ll<- (ave(CTRL1b$ptamt995, CTRL1b$latlon, FUN=function(x) mean(x, na.rm=T))) 
IRRIG1b$ptamt995min<-(ave(IRRIG1b$PRECT, IRRIG1b$latlon, FUN=function(x) quantile(x, 0.995, na.rm=T))) 
IRRIG1b$ptamt995<-ifelse(IRRIG1b$PRECT>= IRRIG1b$ptamt995min, IRRIG1b$PRECT, NA) ###DO FOR EACH LAT LON!!!!!
IRRIG1b$iptamt995ll<- (ave(IRRIG1b$ptamt995, IRRIG1b$latlon, FUN=function(x) mean(x, na.rm=T))) 

#100+ year storms
CTRL1b$ptamt99min<-(ave(CTRL1b$PRECT, CTRL1b$latlon, FUN=function(x) quantile(x, 0.99, na.rm=T))) 
CTRL1b$ptamt99<-ifelse(CTRL1b$PRECT>= CTRL1b$ptamt99min & CTRL1b$PRECT< CTRL1b$ptamt995min, CTRL1b$PRECT, NA) ###DO FOR EACH LAT LON!!!!!
CTRL1b$ptamt99ll<- (ave(CTRL1b$ptamt99, CTRL1b$latlon, FUN=function(x) mean(x, na.rm=T))) 
IRRIG1b$ptamt99min<-(ave(IRRIG1b$PRECT, IRRIG1b$latlon, FUN=function(x) quantile(x, 0.99, na.rm=T))) 
IRRIG1b$ptamt99<-ifelse(IRRIG1b$PRECT>= IRRIG1b$ptamt99min & IRRIG1b$PRECT< IRRIG1b$ptamt995min, IRRIG1b$PRECT, NA) ###DO FOR EACH LAT LON!!!!!
IRRIG1b$iptamt99ll<- (ave(IRRIG1b$ptamt99, IRRIG1b$latlon, FUN=function(x) mean(x, na.rm=T))) 


#500 year storms PRECL
CTRL1b$plamt995min<-(ave(CTRL1b$PRECL, CTRL1b$latlon, FUN=function(x) quantile(x, 0.995, na.rm=T))) 
CTRL1b$plamt995<-ifelse(CTRL1b$PRECL>= CTRL1b$plamt995min, CTRL1b$PRECL, NA) ###DO FOR EACH LAT LON!!!!!
CTRL1b$plamt995ll<- (ave(CTRL1b$plamt995, CTRL1b$latlon, FUN=function(x) mean(x, na.rm=T))) 
IRRIG1b$plamt995min<-(ave(IRRIG1b$PRECL, IRRIG1b$latlon, FUN=function(x) quantile(x, 0.995, na.rm=T))) 
IRRIG1b$plamt995<-ifelse(IRRIG1b$PRECL>= IRRIG1b$plamt995min, IRRIG1b$PRECL, NA) ###DO FOR EACH LAT LON!!!!!
IRRIG1b$iplamt995ll<- (ave(IRRIG1b$plamt995, IRRIG1b$latlon, FUN=function(x) mean(x, na.rm=T))) 

#100+ year storms
CTRL1b$plamt99min<-(ave(CTRL1b$PRECL, CTRL1b$latlon, FUN=function(x) quantile(x, 0.99, na.rm=T))) 
CTRL1b$plamt99<-ifelse(CTRL1b$PRECL>= CTRL1b$plamt99min & CTRL1b$PRECL< CTRL1b$plamt995min, CTRL1b$PRECL, NA) ###DO FOR EACH LAT LON!!!!!
CTRL1b$plamt99ll<- (ave(CTRL1b$plamt99, CTRL1b$latlon, FUN=function(x) mean(x, na.rm=T))) 
IRRIG1b$plamt99min<-(ave(IRRIG1b$PRECL, IRRIG1b$latlon, FUN=function(x) quantile(x, 0.99, na.rm=T))) 
IRRIG1b$plamt99<-ifelse(IRRIG1b$PRECL>= IRRIG1b$plamt99min & IRRIG1b$PRECL< IRRIG1b$plamt995min, IRRIG1b$PRECL, NA) ###DO FOR EACH LAT LON!!!!!
IRRIG1b$iplamt99ll<- (ave(IRRIG1b$plamt99, IRRIG1b$latlon, FUN=function(x) mean(x, na.rm=T))) 


IRRIG1b$dif995<-IRRIG1b$ptamt995-CTRL1b$ptamt995ll
IRRIG1b$dif995p<-(ave(IRRIG1b$dif995, IRRIG1b$latlon, FUN=function(x) t.test(x[is.na(x)==F])$p.value[[1]] )) 
#PRECL ttest
IRRIG1b$dif995l<-IRRIG1b$plamt995-CTRL1b$plamt995ll
IRRIG1b$dif995lp<-(ave(IRRIG1b$dif995l, IRRIG1b$latlon, FUN=function(x) t.test(x[is.na(x)==F])$p.value[[1]] )) 


IDmean<-subset(IRRIG1b, !duplicated(latlon)) #,  select=c(lon, lat, latlon, iptamt999ll))
CTmean<-subset(CTRL1b, !duplicated(latlon)) #, select=c(lon, lat, latlon, ptamt995ll))

IDmean$DIF99<-IDmean$iptamt99ll-CTmean$ptamt99ll
IDmean$DIF995<-IDmean$iptamt995ll-CTmean$ptamt995ll
IDmean$DIF99pct<-((IDmean$iptamt99ll-CTmean$ptamt99ll)/CTmean$ptamt99ll)*100
IDmean$DIF995pct<-((IDmean$iptamt995ll-CTmean$ptamt995ll)/CTmean$ptamt995ll)*100


#precl
IDmean$DIF99l<-IDmean$iplamt99ll-CTmean$plamt99ll
IDmean$DIF995l<-IDmean$iplamt995ll-CTmean$plamt995ll
IDmean$DIF99lpct<-((IDmean$iplamt99ll-CTmean$plamt99ll)/CTmean$plamt99ll)*100
IDmean$DIF995lpct<-((IDmean$iplamt995ll-CTmean$plamt995ll)/CTmean$plamt995ll)*100

#IDmean22<-subset(IDmean, latlon %in% c(40740285, 40740287.5,  42630285, 42630287.5 )) #42630282.5,
#CTmean22<-subset(CTmean, latlon %in% c(40740285, 40740287.5,  42630285, 42630287.5 )) #42630282.5,

#write.csv(IDmean, "~/spunupF2EXP3/500yramtUSA.csv" )
write.csv(IDmean, "~/Users/travis/GitHub/IRRIG/CESM.summary.data/500yramtUSAPRECLextreme.csv" )




#######
############ #DATA ANALYSIS my comp.
########################################################################################################
#
install.packages("mFilter",  repos = "http://cran.case.edu" )  # Sys.getenv("R_LIBS_USER"),


library(data.table)
library(ggmap)
library(gplots)
library(plotrix)
library(ggplot2)
library(maps)
library(ncdf)
library(chron)
library(sp)
library(data.table)
library(reshape2)
library(foreach)
library(plyr)
library(SDMTools)
library(grid)
library(gridExtra)
library(plotrix)
library(grDevices)
library(mFilter)


#I500DIFF <-as.data.table(read.csv("~/500yramtUSA.csv", header = TRUE, sep = ",", quote="\"", dec="."))
I500DIFF <-as.data.table(read.csv("~/500yramtUSAPRECL.csv", header = TRUE, sep = ",", quote="\"", dec="."))

I500DIFF$lon2<- I500DIFF$lon-360

#I500DIFF$dif995p2<-ifelse(I500DIFF$dif995p>0.05, 0.95, 1)
I500DIFF$dif995lp2<-ifelse(I500DIFF$dif995lp>0.05, 0.95, 1)

I500DIFF2<-subset( I500DIFF,lon2> -100 & lat<50 &lat>40 )
us_states <- map_data("state")
s<-(ggplot(aes(x=lon2, y=lat, fill=(DIF995lpct)), data= I500DIFF2) + geom_tile())+geom_polygon(data=us_states, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0) + scale_fill_gradient2(low = "pink", high = "green") 
s
#s<- s+ geom_tile(aes(x=lon2, y=lat, fill=(dif995p),  alpha=(dif995p2)), data= I500DIFF2) + scale_fill_gradient2(low = "grey", high = "white") 
#use daily data to see how much July PRECL is per day and total to see what a 3mm means...







#######################################################################################################
############STORM TRACKS!!!


####IRRIG file
IRRIG1 <-as.data.table(read.csv("~/spunupF2EXP3/P2IRRIGdayPRECZ10.110.csv", header = TRUE, sep = ",", quote="\"", dec="."))
setnames(IRRIG1, c("X2", "lon", "lat", "day", "PRECL", "PRECC","Z500", "cond", "month"))

IRRIG1$latlon<-IRRIG1$lat*1000000+IRRIG1$lon
IRRIG1$monday<-IRRIG1$month*1000+IRRIG1$day

#select lat lon with greatest PRECL change
st2<-subset(IRRIG1, latlon %in% c(40740285, 40740287.5,  42630285, 42630287.5 )) #42630282.5,

st2$PRECC<-st2$PRECC*86400000 #convert to mm/day
st2$PRECL<-st2$PRECL*86400000
st2$PRECT<- st2$PRECC+ st2$PRECL

st2$ptamt995<-ifelse(st2$PRECT>=quantile(st2$PRECT, 0.995) & st2$PRECT<quantile(st2$PRECT, 0.999), st2$PRECT, NA)
st2$ptamt999<-ifelse(st2$PRECT>=quantile(st2$PRECT, 0.999), st2$PRECT, NA)

st3<-subset(st2, ptamt999>0 )

#select storm days
st3<-subset(IRRIG1, monday>= 110010 &  monday<= 110015 ) #154003&154010  #110005&110018 #72010&72021 130009&130017 187005 &  monday<= 187013

write.csv(st3, "~/spunupF2EXP3/storm1.csv" )


###analysis office
####IRRIG file
ST1 <-as.data.table(read.csv("~/storm1.csv", header = TRUE, sep = ",", quote="\"", dec="."))
ST1$Zve<- (ave(ST1$Z500, ST1$latlon, FUN=function(x) mean(x, na.rm=T))) 
ST1$z2<-ST1$Z500-ST1$Zve

md<-c(110010: 110015) #c(110018:110005) #(154010: 154003) #c(72010: 72021) 130009: 130017 #187005: 187013 #north:110022: 110030 #midwest:110010: 110015

for (s in 1:20){
st2<-subset(ST1, monday== md[s] & lat<50)
st2$lon2<- st2$lon-360

us_states <- map_data("state")
s<-(ggplot(aes(x=lon2, y=lat, fill=(PRECL)), data= st2) + geom_tile())+geom_polygon(data=us_states, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0) + scale_fill_gradient2(low = "pink", high = "green") 
print(s)

}

#+ scale_fill_gradient2(low = "pink", high = "green") 
#+ scale_fill_gradient2(low = "pink", high = "green") 














#######################################################################################################
############STORM SD Z500!!!


####IRRIG file
CTRL1<-as.data.table(read.csv("~/spunupF2EXP3/P2CTRLdayPRECZ10.110.csv", header = TRUE, sep = ",", quote="\"", dec="."))
setnames(CTRL1, c("X2","lon", "lat", "day", "PRECL", "PRECC","Z500", "cond", "month"))

IRRIG1 <-as.data.table(read.csv("~/spunupF2EXP3/P2IRRIGdayPRECZ10.110.csv", header = TRUE, sep = ",", quote="\"", dec="."))
setnames(IRRIG1, c("X2", "lon", "lat", "day", "PRECL", "PRECC","Z500", "cond", "month"))

IRRIG1$latlon<-IRRIG1$lat*1000000+IRRIG1$lon
IRRIG1$z500SD<- (ave(IRRIG1$Z500, IRRIG1$latlon, FUN=function(x) sd(x, na.rm=T))) 

CTRL1$latlon<-CTRL1$lat*1000000+ CTRL1$lon
CTRL1$z500SD<- (ave(CTRL1$Z500, CTRL1$latlon, FUN=function(x) sd(x, na.rm=T))) 
IRRIG1$z500SDdiff<-IRRIG1$z500SD-CTRL1$z500SD
#IRRIG1$z500SDttest <-(ave(IRRIG1$z500SDdiff, IRRIG1$latlon, FUN=function(x) t.test(x)$p.value[[1]] )) 

Z500diff<-subset(IRRIG1, !duplicated(latlon), select = c( lat, lon, z500SDdiff))

write.csv(Z500diff, "~/spunupF2EXP3/Z500diff.csv" )


IRRIG1$Z500noPRECC<- ifelse( IRRIG1$PRECC > (2.050e-10), NA, IRRIG1$Z500)
IRRIG1$z500SDnoPRECC<- (ave(IRRIG1$Z500noPRECC, IRRIG1$latlon, FUN=function(x) sd(x, na.rm=T))) 

CTRL1$Z500noPRECC<- ifelse( CTRL1$PRECC > (2.050e-10), NA, CTRL1$Z500)
CTRL1$z500SDnoPRECC<- (ave(CTRL1$Z500noPRECC, CTRL1$latlon, FUN=function(x) sd(x, na.rm=T))) 
IRRIG1$z500SDdiffnoPRECC<-IRRIG1$z500SDnoPRECC-CTRL1$z500SDnoPRECC

Z500diffnoPRECC<-subset(IRRIG1, !duplicated(latlon), select = c( lat, lon, z500SDdiffnoPRECC))
write.csv(Z500diffnoPRECC, "~/spunupF2EXP3/Z500diffnoPRECC.csv" )



# large storms

CTRL1$latlon<-CTRL1$lat*1000000+ CTRL1$lon
IRRIG1$latlon<-IRRIG1$lat*1000000+IRRIG1$lon

CTRL1$PRECT<- CTRL1$PRECL +CTRL1$PRECC
CTRL1$PTheavy<- (ave(CTRL1$PRECT, CTRL1$latlon, FUN=function(x)  quantile(x, 0.995))) 
CTRL1$Z500PRECL<- ifelse( CTRL1$PRECT> CTRL1$PTheavy, CTRL1$Z500, NA)
CTRL1$z500SDPRECL<- (ave(CTRL1$Z500PRECL, CTRL1$latlon, FUN=function(x) sd(x, na.rm=T))) 


IRRIG1$PRECTirrig<- IRRIG1$PRECL + IRRIG1$PRECC
IRRIG1$Z500irrig<- IRRIG1$Z500
IRRIG1$PTheavyirrig<- (ave(IRRIG1$PRECTirrig, IRRIG1$latlon, FUN=function(x)  quantile(x, 0.995))) 

IRRIG1a<-merge(IRRIG1, CTRL1, by=c("month", "day", "latlon", "lat", "lon"))
IRRIG1a$Z500PRECLirrig<- ifelse( IRRIG1a$PRECTirrig > IRRIG1a$PTheavyirrig, IRRIG1a$Z500irrig, NA)
IRRIG1a$z500SDPRECLirrig<- (ave(IRRIG1a$Z500PRECLirrig, IRRIG1a$latlon, FUN=function(x) sd(x, na.rm=T))) 

#IRRIG1$z500SDPRECL<- (ave(IRRIG1$Z500PRECL, IRRIG1$latlon, FUN=function(x) sd(x, na.rm=T))) 

IRRIG1a$z500SDdiffPRECL<-IRRIG1a$z500SDPRECLirrig-IRRIG1a$z500SDPRECL

Z500diffPRECL<-subset(IRRIG1a, !duplicated(latlon), select = c( lat, lon, z500SDdiffPRECL))
write.csv(Z500diffPRECL, "~/spunupF2EXP3/Z500diffPRECL.csv" )



##bandpassed!
CTRL1<-as.data.table(read.csv("~/spunupF2EXP3/P2CTRLdayPRECZ10.110.csv", header = TRUE, sep = ",", quote="\"", dec="."))
setnames(CTRL1, c("X2","lon", "lat", "day", "PRECL", "PRECC","Z500", "cond", "month"))

bkFunc <- function(x){
bk<- (bkfilter(x, pl = 2, pu = 6))
#return(sd(bk$cycle, na.rm=T))
#return(cbind(sd(bk$cycle, na.rm=T), mean(bk$trend, na.rm=T), sd(bk$trend, na.rm=T)))
return(sd(bk$trend, na.rm=T))
#return(bk)
}

CTRL1$latlon<-CTRL1$lat*1000000+ CTRL1$lon
CTRL2<-subset(CTRL1, lon> 260 & lon<295 & lat>20 & lat<52 , select=c(latlon, Z500))
#CTRL2<-subset(CTRL1,  lon==230 & lat>20 & lat<52, select=c(latlon, Z500))
#CTRL3<-ddply(CTRL2, "latlon", bkFunc)
CTRL3<-aggregate(Z500~latlon, data= CTRL2 , bkFunc)


IRRIG1 <- as.data.table(read.csv("~/spunupF2EXP3/P2IRRIGdayPRECZ10.110.csv", header = TRUE, sep = ",", quote="\"", dec="."))
setnames(IRRIG1, c("X2", "lon", "lat", "day", "PRECL", "PRECC","Z500", "cond", "month"))

IRRIG1$latlon<-IRRIG1$lat*1000000+ IRRIG1$lon
IRRIG1$Z500irrig <- IRRIG1$Z500
IRRIG2 <- subset(IRRIG1, lon> 260 & lon<295 & lat>22 & lat<52, select=c(latlon, Z500irrig))
IRRIG3 <- aggregate(Z500irrig~latlon, data= IRRIG2 , bkFunc)
CImerge<- merge(CTRL3, IRRIG3, by="latlon")

IRRIG1a<-subset(IRRIG1, !duplicated(latlon), select = c(latlon, lat, lon))
Z500merge<- merge(CImerge, IRRIG1a, by="latlon")
Z500merge$z500SDdiff<-Z500merge$Z500irrig-Z500merge$Z500
Z500merge<-subset(Z500merge, !duplicated(latlon), select = c(z500SDdiff, lat, lon))

write.csv(Z500merge, "~/spunupF2EXP3/Z500diffbf.csv" )




##bandpassed! EXTREMES

bkFuncX <- function(x){
bk<- (bkfilter(x$Z500, pl = 2, pu = 6))
return(cbind(bk$cycle, x$PRECT))
}
CTRL1$PRECT<- CTRL1$PRECL #+CTRL1$PRECC
CTRL1$latlon<-CTRL1$lat*1000000+ CTRL1$lon
CTRL2<-subset(CTRL1, lon> 260 & lon<295 & lat>20 & lat<52 , select=c(latlon, Z500, PRECT))
CTRL3<-ddply(CTRL2, "latlon", bkFuncX)
#CTRL3<-aggregate(Z500~latlon, data= CTRL2 , bkFunc)
setnames(CTRL3, c("latlon", "Z500bk", "PRECT"))
CTRL3$PRECT99 <- ave(CTRL3$PRECT, CTRL3$latlon, FUN=function(x) quantile(x, 0.995, na.rm=T))

CTRL4<-subset(CTRL3, PRECT>= PRECT99 , select=c(latlon, Z500bk))
CTRL4$Z500BK99<- ave(CTRL4$Z500bk, CTRL4$latlon, FUN=function(x) sd(x, na.rm=T))
CTRL4<-subset(CTRL4,!duplicated(latlon))


#IRRIG1 <- as.data.table(read.csv("~/spunupF2EXP3/P2IRRIGdayPRECZ10.110.csv", header = TRUE, sep = ",", quote="\"", dec="."))
#setnames(IRRIG1, c("X2", "lon", "lat", "day", "PRECL", "PRECC","Z500", "cond", "month"))

IRRIG1$latlon<-IRRIG1$lat*1000000+ IRRIG1$lon
IRRIG1$Z500irrig <- IRRIG1$Z500

IRRIG1$PRECT<- IRRIG1$PRECL #+IRRIG1$PRECC
IRRIG1$latlon<-IRRIG1$lat*1000000+ IRRIG1$lon
IRRIG2<-subset(IRRIG1, lon> 260 & lon<295 & lat>20 & lat<52 , select=c(latlon, Z500, PRECT))
IRRIG3<-ddply(IRRIG2, "latlon", bkFuncX)
#IRRIG3<-aggregate(Z500~latlon, data= IRRIG2 , bkFunc)
setnames(IRRIG3, c("latlon", "Z500bk", "PRECT"))
#IRRIG3$PRECT99 <- ave(IRRIG3$PRECT, IRRIG3$latlon, FUN=function(x) quantile(x, 0.995, na.rm=T))
IRRIG3$PRECT99 <- ave(CTRL3$PRECT, CTRL3$latlon, FUN=function(x) quantile(x, 0.995, na.rm=T))

IRRIG4<-subset(IRRIG3, PRECT>= PRECT99 , select=c(latlon, Z500bk))
IRRIG4$Z500BK99irrig<- ave(IRRIG4$Z500bk, IRRIG4$latlon, FUN=function(x) sd(x, na.rm=T))
IRRIG4<-subset(IRRIG4,!duplicated(latlon))


CImerge<- merge(CTRL4, IRRIG4, by="latlon")

IRRIG1a<-subset(IRRIG1, !duplicated(latlon), select = c(latlon, lat, lon))
Z500merge<- merge(CImerge, IRRIG1a, by="latlon")
Z500merge$z500SDdiff<-Z500merge$Z500BK99irrig-Z500merge$Z500BK99
Z500merge<-subset(Z500merge, !duplicated(latlon), select = c(z500SDdiff, lat, lon))


# # IRRIG2 <- subset(IRRIG1, lon> 260 & lon<295 & lat>22 & lat<52, select=c(latlon, Z500irrig))
# IRRIG3 <- aggregate(Z500irrig~latlon, data= IRRIG2 , bkFunc)
# CImerge<- merge(CTRL3, IRRIG3, by="latlon")

# IRRIG1a<-subset(IRRIG1, !duplicated(latlon), select = c(latlon, lat, lon))
# Z500merge<- merge(CImerge, IRRIG1a, by="latlon")
# Z500merge$z500SDdiff<-Z500merge$Z500irrig-Z500merge$Z500
# Z500merge<-subset(Z500merge, !duplicated(latlon), select = c(z500SDdiff, lat, lon))

write.csv(Z500merge, "~/spunupF2EXP3/Z500diffbf.csv" )






#
#analysis on my comp.
STSD1 <-as.data.table(read.csv("~/Z500diffbf.csv", header = TRUE, sep = ",", quote="\"", dec="."))

STSD1$lon2<- STSD1$lon-360
#STSD1$z500SDdiff2<-ifelse(STSD1$z500SDdiff>0,(STSD1$z500SDdiff)^0.7, 0 )
#STSD1$z500SDdiff2<-ifelse(STSD1$z500SDdiffnoPRECC>0,(STSD1$z500SDdiffnoPRECC)^0.5, 0 )
#STSD1$z500SDdiff2<-ifelse(STSD1$z500SDdiffPRECL>0,(STSD1$z500SDdiffPRECL)^0.1, 0 )


dev.new(width=5, height=4)
#STSD2<-subset(STSD1, lon>280 & lon<292 & lat>41 & lat<48)

world <- map_data("world")
us_states <- map_data("state")
(ggplot(aes(x=lon2, y=lat, fill=(z500SDdiff)), data= STSD1) + geom_tile())+geom_polygon(data=us_states, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0) + scale_fill_gradientn(colours= rev(c("tomato3","tomato2", "tomato","seashell", "slategray1", "slategray2")), space = "Lab", na.value = "grey50",  guide = "colourbar", guide_colourbar(title="500mb SD"))+ geom_polygon(data=world, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0) + theme(axis.line = element_line( size = 0.35)) + theme(axis.ticks.margin = unit(0.06, "cm")) + theme(axis.ticks.length = unit(0.1, "cm")) +theme( panel.background = element_rect(fill=NA),  panel.grid.major = element_blank(),panel.grid.minor = element_blank(), plot.background = element_rect(fill=NA))    + theme(axis.text.y = element_text(angle = 90, hjust = 0.5, size=11, colour="black"))+ theme(axis.text.x = element_text(  size=11, colour="black")) +scale_y_continuous(name=(expression(paste("Latitude (", degree, "N)")))) +scale_x_continuous(name=(expression(paste("Longitude (", degree, "E)")))) + coord_cartesian(xlim = c(-100, -63), ylim = c(24, 50)) 


#+ scale_fill_gradientn(colours= rev(c("tomato4", "tomato3","tomato3","tomato2","tomato","seashell","slategray1", "slategray2"  ,"slategray3", "royalblue2", "royalblue3", "royalblue3", "royalblue3" )), space = "Lab", na.value = "grey50",  guide = "colourbar", guide_colourbar(title="500mb SD"))
