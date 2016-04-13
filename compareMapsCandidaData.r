library(fields)
library(ncdf4)
library(raster)
library(rgdal)
library(RColorBrewer)
library(viridis)

source("C:\\GoogleDrive\\Climate\\Rcode\\my.image.plot.r")
source("C:\\GoogleDrive\\Climate\\Rcode\\my.filled.contour.r")
OutputGraphics<-"C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\graphics"
latName <- c("latitude",rep("lat",times=8))
lonName <- c("longitude",rep("lon",times=8))
years <- 1988:1999
ImgLst <- list() 

BasePaths=c(cmip5Obs = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\cmip5\\hydro\\historical_mon_VIC\\conus_c5.para_v0.monthly",
cmip3arkred = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\arkred\\obs.maurer_2002.arkred.monthly",
cmip3cali = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\cali\\obs.maurer_2002.cali.monthly",
cmip3colo = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\colo\\obs.maurer_2002.colo.monthly", cmip3gbas = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\gbas\\obs.maurer_2002.gbas.monthly",
cmip3gulf = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\gulf\\obs.maurer_2002.gulf.monthly",
cmip3mo = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\mo\\obs.maurer_2002.mo.monthly",
cmip3pnw = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\pnw\\obs.maurer_2002.pnw.monthly",
cmip3riog = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\riog\\obs.maurer_2002.riog.monthly")
 Vars<-"swe"
 v=1
#Vars<-c("runoff","swe","smc","et","petwater")
CandidaData<-"C:\\Users\\mtalbert\\Downloads\\SWE_monthly_CONUS_12km_1987-2003.nc"
SnowSites<-read.csv("C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\SnotelLocs.csv")
 Paths<-paste(BasePaths,".",Vars[v],".",sep="")
 for(p in 1:length(Paths)){
 VarNames<-Vars[v]
 if(Vars[v]=="runoff" & p==1){
   Paths[p]<-gsub("runoff","total_runoff",Paths[p])
   VarNames<-"total_runoff"
 }
   MonthlyImages <- list()
   for(m in 1:12){
       for(yr in years){
           filePath <- paste(Paths[p],yr,".nc",sep="") 

           Clim <- nc_open(filePath)
      
               if(yr==years[1]){ #do this for the first year only because it takes a bit to caluclate
                 Lat <- ncvar_get(Clim,latName[p])
                 Lon <- ncvar_get(Clim,lonName[p])            
                 OutputImg <- matrix(data=0,nrow=length(Lon),ncol=length(Lat))
              }          
               RastArray <- ncvar_get(Clim, varid=VarNames, start=c(1,1,m),
                           count=c(length(Lon),length(Lat),1))

               #average over space and add to the time series           
                OutputImg <- OutputImg+RastArray
               nc_close(Clim)
        }
        MonthlyImages[[m]] <- OutputImg/length(years)
    }
    ImgLst[[p]] <- MonthlyImages  
 }
#The above could be fairly easilly generalized but now I have to get the images to match
#by figuring out how the coordinates of the smaller fit into the larger
   p=1
   filePath <- paste(Paths[p],yr,".nc",sep="")
   Clim <- nc_open(filePath)
   countryLat<- ncvar_get(Clim,latName[p])
   countryLon <- ncvar_get(Clim,lonName[p])
   nc_close(Clim)

 for(p in 2:length(Paths)){
   filePath <- paste(Paths[p],yr,".nc",sep="")
   Clim <- nc_open(filePath)
   regionLat<- ncvar_get(Clim,latName[p])
   regionLon <- ncvar_get(Clim,lonName[p])
   nc_close(Clim)

   OutputImg <- matrix(data=0,nrow=length(countryLon),ncol=length(countryLat))
   LonRng<-which(regionLon[1]==countryLon,arr.ind=TRUE):which(regionLon[length(regionLon)]==countryLon,arr.ind=TRUE)
   LatRng<-which(regionLat[1]==countryLat,arr.ind=TRUE):which(regionLat[length(regionLat)]==countryLat,arr.ind=TRUE)
   for(m in 1:12){
     OutputImg <- matrix(data=NA,nrow=length(countryLon),ncol=length(countryLat))
     OutputImg[LonRng,LatRng]<-ImgLst[[p]][[m]]
     ImgLst[[p]][[m]]<-OutputImg
   }
}
#I'm cobining the three small regions into one region by summing an array with na remove and then
#masking out areas that were NA in all three
vic407<-list()
for(m in 1:12){ #months
d<-array(data=NA,c(length(countryLon),length(countryLat),8))
 #put the three regions in my array
 for(rgn in 1:8){ d[,,rgn]<-ImgLst[[(rgn+1)]][[m]]}

combineMap<-apply(d,c(1,2),mean,na.rm=TRUE)
f<-function(x){sum(is.na(x))==8}
NAmask<-apply(d,c(1,2),FUN=f)
NAmask[NAmask==TRUE]<-NA
vic407[[m]]<-combineMap+NAmask
}
 

monthlySatellite<-list()

Clim<-nc_open(CandidaData)
 for(m in 1:12){
       for(yr in 1:length(years)){
       if(yr==1){ #do this for the first year only because it takes a bit to caluclate
                 satLat <- ncvar_get(Clim,"lat")
                 satLon <- ncvar_get(Clim,"lon")
                 Time<-ncvar_get(Clim,"time")
                 OutputImg <- array(data=0,c(length(years),length(satLon),ncol=length(satLat)))
              }
       ind<-as.vector(which(as.character(Time)==paste(years[yr],ifelse(m<10,"0",""),m,sep=""),arr.ind=TRUE))
       print(paste(years[yr],ifelse(m<10,"0",""),m,sep=""))
       print(ind)
         RastArray<-ncvar_get(Clim,varid="swe",start=c(1,1,ind),
                           count=c(length(satLon),length(satLat),1))
          OutputImg[yr,,] <- RastArray
                           
       }
   monthlySatellite[[m]]<-apply(OutputImg,c(2,3),mean,na.rm=TRUE)
   f<-function(x,count){sum(is.na(x))==count}
NAmask<-apply(OutputImg,c(2,3),FUN=f,count=length(years))
NAmask[NAmask==TRUE]<-NA
monthlySatellite[[m]]<-monthlySatellite[[m]]+NAmask
  }
nc_close(Clim)

vic412<-ImgLst[[1]]


for(m in c(1,2,3,11,12)){
    #==============================
    #plotting the three ratios both log and nonlog scale non log I take the greater of the exp(log(ratio)) and add a sign
    #to indicate which was greater
    Colors<-c(colorRampPalette(c("blue","grey96"))(50),"grey96","grey96",rev(colorRampPalette(c("red4","grey96"))(50)))
    breakRng<-c(-2,2)
    r<-max(abs(breakRng))
    Breaks<-seq(from=-r,to=r,length=length(Colors)+1)

    absbreakRng<-c(-3,3)
    absBreaks<-seq(from=absbreakRng[1],to=absbreakRng[2],length=length(Colors)+1)

   MonthlyDiff<-log((vic412[[m]]+10)/(monthlySatellite[[m]][2:(nrow(monthlySatellite[[m]])-1),2:(ncol(monthlySatellite[[m]])-1)]+10))
   myImagePlot(MonthlyDiff,"SWE","log((CMIP5 VIC + 10)/(Satellite+10))","CMIP5Satellite",m,breakRng,
      Breaks,countryLat,countryLon,OutputGraphics,Colors,SnowSites,LogToAbs=TRUE)
   NonLogScale<-exp(MonthlyDiff)*(exp(MonthlyDiff)>exp(-MonthlyDiff))+(-exp(-MonthlyDiff)*(exp(MonthlyDiff)<=exp(-MonthlyDiff)))
   myImagePlot(NonLogScale,"SWE","(CMIP5 VIC + 10)/(Satellite+10)","NonLogCMIP5Satellite",m,absbreakRng,absBreaks,countryLat,
        countryLon,OutputGraphics,Colors,SnowSites)
   
   #Satellite data is recorded mid month so maybe we should compare to the mean of this month and next month in the VICs
   #this actually does much worse for some reason...
   mean2Months<-(vic412[[m]]+vic412[[ifelse(m==12,1,m+1)]])/2
   MonthlyDiff<-log((mean2Months+10)/(monthlySatellite[[m]][2:(nrow(monthlySatellite[[m]])-1),2:(ncol(monthlySatellite[[m]])-1)]+10))
   myImagePlot(MonthlyDiff,"SWE","log((CMIP5 VIC + 10)/(Satellite+10))","CMIP5SatelliteMean2Months",
      m,breakRng,Breaks,countryLat,countryLon,OutputGraphics,Colors,SnowSites)
   NonLogScale<-exp(MonthlyDiff)*(exp(MonthlyDiff)>exp(-MonthlyDiff))+(-exp(-MonthlyDiff)*(exp(MonthlyDiff)<=exp(-MonthlyDiff)))
   myImagePlot(MonthlyDiff,"SWE","(CMIP5 VIC + 10)/(Satellite+10)","NonLogCMIP5SatelliteMean2Months",m,absbreakRng,absBreaks,countryLat,
        countryLon,OutputGraphics,Colors,SnowSites,LogToAbs=TRUE)
   
   
   MonthlyDiff<-log((vic407[[m]]+10)/(monthlySatellite[[m]][2:(nrow(monthlySatellite[[m]])-1),2:(ncol(monthlySatellite[[m]])-1)]+10))
   #myImagePlot(MonthlyDiff,"SWE","log((CMIP3 VIC + 10)/(Satellite+10))","CMIP3Satellite",m,breakRng,Breaks,countryLat,countryLon,OutputGraphics,Colors,SnowSites)
   NonLogScale<-exp(MonthlyDiff)*(exp(MonthlyDiff)>exp(-MonthlyDiff))+(-exp(-MonthlyDiff)*(exp(MonthlyDiff)<=exp(-MonthlyDiff)))
   myImagePlot(NonLogScale,"SWE","(CMIP3 VIC + 10)/(Satellite+10)","NonLogCMIP5Satellite",m,absbreakRng,absBreaks,countryLat,
        countryLon,OutputGraphics,Colors,SnowSites)
        
   MonthlyDiff<-log((vic412[[m]]+10)/(vic407[[m]]+10))
   #myImagePlot(MonthlyDiff,"SWE","log((CMIP5 VIC + 10)/(CMIP3 VIC + 10))","CMIP5CMIP3",m,breakRng,Breaks,countryLat,countryLon,OutputGraphics,Colors,SnowSites)
   NonLogScale<-exp(MonthlyDiff)*(exp(MonthlyDiff)>exp(-MonthlyDiff))+(-exp(-MonthlyDiff)*(exp(MonthlyDiff)<=exp(-MonthlyDiff)))
   myImagePlot(NonLogScale,"SWE","(CMIP5 VIC + 10)/(CMIP3 VIC+10)","NonLogCMIP5CMIP3",m,absbreakRng,absBreaks,countryLat,
        countryLon,OutputGraphics,Colors,SnowSites)
   #==============================
   #now plotting the three data sets
   Colors   = magma(50,begin=1,end=0)
   breakRng<-c(quantile(vic412[[m]],na.rm=TRUE,probs=c(.05,.95)),
               quantile(vic407[[m]],na.rm=TRUE,probs=c(.05,.95)),
               quantile(monthlySatellite[[m]],na.rm=TRUE,probs=c(.05,.95)))
   r<-max(abs(breakRng))
   Breaks<-seq(from=min(breakRng),to=max(breakRng),length=length(Colors)+1)
   
   MonthlyDiff<-vic412[[m]]
   myImagePlot(MonthlyDiff,"SWE","CMIP5","CMIP5",m,breakRng,Breaks,countryLat,countryLon,OutputGraphics,Colors)
    
   MonthlyDiff<-vic407[[m]]
   myImagePlot(MonthlyDiff,"SWE","CMIP3","CMIP3",m,breakRng,Breaks,countryLat,countryLon,OutputGraphics,Colors)
   
   MonthlyDiff<-monthlySatellite[[m]]
   myImagePlot(MonthlyDiff[2:(nrow(monthlySatellite[[m]])-1),2:(ncol(monthlySatellite[[m]])-1)],"SWE","Satelite",
   "Satelite",m,breakRng,Breaks,countryLat,countryLon,OutputGraphics,Colors)
}

