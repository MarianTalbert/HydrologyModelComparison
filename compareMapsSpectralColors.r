library(fields)
library(ncdf4)
library(raster)
library(rgdal)

source("C:\\GoogleDrive\\Climate\\Rcode\\my.image.plot.r")
source("C:\\GoogleDrive\\Climate\\Rcode\\my.filled.contour.r")
OutputGraphics<-"C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\graphics2"
latName <- c("latitude",rep("lat",times=8))
lonName <- c("longitude",rep("lon",times=8))
years <- 1950:1999
ImgLst <- list() 

BasePaths=c(cmip5Obs = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\cmip5\\hydro\\historical_mon_VIC\\conus_c5.para_v0.monthly",
cmip3arkred = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\arkred\\obs.maurer_2002.arkred.monthly",
cmip3cali = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\cali\\obs.maurer_2002.cali.monthly",
cmip3colo = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\colo\\obs.maurer_2002.colo.monthly", cmip3gbas = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\gbas\\obs.maurer_2002.gbas.monthly",
cmip3gulf = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\gulf\\obs.maurer_2002.gulf.monthly",
cmip3mo = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\mo\\obs.maurer_2002.mo.monthly",
cmip3pnw = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\pnw\\obs.maurer_2002.pnw.monthly",
cmip3riog = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\riog\\obs.maurer_2002.riog.monthly")

Vars<-c("runoff","swe","smc","et","petwater")

for(v in 1:length(Vars)){
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
par(mfrow=c(2,2))  
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
combineMapLst<-list()
for(m in 1:12){ #months
d<-array(data=NA,c(length(countryLon),length(countryLat),8))
 #put the three regions in my array
 for(rgn in 1:8){ d[,,rgn]<-ImgLst[[(rgn+1)]][[m]]}

combineMap<-apply(d,c(1,2),sum,na.rm=TRUE) 
f<-function(x){sum(is.na(x))==8}
NAmask<-apply(d,c(1,2),FUN=f)
NAmask[NAmask==TRUE]<-NA
combineMapLst[[m]]<-combineMap+NAmask 
}



for(m in 1:12){
    Colors<-c(colorRampPalette(c("blue","grey92")(50),rev(colorRampPalette(c("red4","grey92"))(50)))

    #I think this ramp looks like a clown but what do I know...
    #Colors   = c(colorRampPalette(c("darkorchid1","blue","green4","green","palegreen", "grey95"))(50),
    #rev(colorRampPalette(c("red4","red","orange","yellow","lemonchiffon","grey95"))(50)))
    jet.colors <- colorRampPalette(Colors)
#===================================
#Ratio Here
#===================================
    png(file.path(OutputGraphics,paste(Vars[v],month.abb[m],"VicLogRatio.png",sep="")),width=1000,
                      height=700)
    MonthlyDiff<-log((ImgLst[[1]][[m]]+2)/(combineMapLst[[m]]+2)) #I'm adding the 1 to avoid division by zero as well as
    #Sgn<-sign(MonthlyDiff)
    #MonthlyDiff<-Sgn*exp(abs(MonthlyDiff))
    #small values leading to large differences
       # breakRng<-range(MonthlyDiff,na.rm=TRUE)
       #I think I want the break range consistent across all plots
         breakRng<-quantile(MonthlyDiff,na.rm=TRUE,probs=c(.02,.98))
         MonthlyDiff[MonthlyDiff<min(breakRng)]<-min(breakRng)
         MonthlyDiff[MonthlyDiff>max(breakRng)]<-max(breakRng)
         r<-max(abs(breakRng))
         Breaks<-seq(from=-r,to=r,length=length(Colors)+1)
        par(mar=c(6,5,6,6))
         my.filled.contour(x=countryLon,y=countryLat,z=MonthlyDiff,zlim=c(-r,r),color.palette=jet.colors,
               plot.axes = {map("state",lwd=2,col="grey",add=TRUE) },
                 xlab="Longitude",
                 ylab="Latitude",main=paste(month.name[m],toupper(Vars[v]),"log((CMIP5 VIC + 2)/(CMIP3 VIC + 2))"),
                 cex.lab=2,cex.main=2)
           par(oma=c( 0,1,0,0))
             my.image.plot(MonthlyDiff,legend.only=TRUE,col=jet.colors(length(Breaks)-1),breaks=Breaks,zlim=c(-r,r),cex.axis=1.5)
    dev.off()
    
#===================================
#log ratio plotted but transform on the legend bar
#===================================
    png(file.path(OutputGraphics,paste(Vars[v],month.abb[m],"VicRatioScale.png",sep="")),width=1000,
                      height=700)
    MonthlyDiff<-(ImgLst[[1]][[m]]+2)/(combineMapLst[[m]]+2) #I'm adding the 1 to avoid division by zero as well as
    #Sgn<-sign(MonthlyDiff)
    #MonthlyDiff<-Sgn*exp(abs(MonthlyDiff))
    #small values leading to large differences
       # breakRng<-range(MonthlyDiff,na.rm=TRUE)
       #I think I want the break range consistent across all plots
         breakRng<-quantile(MonthlyDiff,na.rm=TRUE,probs=c(.02,.98))
         MonthlyDiff[MonthlyDiff<min(breakRng)]<-min(breakRng)
         MonthlyDiff[MonthlyDiff>max(breakRng)]<-max(breakRng)
         r<-max(abs(breakRng))
         Breaks<-seq(from=-r,to=r,length=length(Colors)+1)

         layout(matrix(c(1,2), 1, 2),c(6,1))
         par(oma=c(0,2,2,0),mar=c(2,2,2,2))
         my.filled.contour(x=countryLon,y=countryLat,z=MonthlyDiff,zlim=c(-r,r),color.palette=jet.colors,
               plot.axes = {map("state",lwd=2,col="grey",add=TRUE) },
                 xlab="Longitude",
                 ylab="Latitude",main=paste(month.name[m],toupper(Vars[v]),"log((CMIP5 VIC + 2)/(CMIP3 VIC + 2))"),
                 cex.lab=2,cex.main=2)

           plot(c(0,1),extendrange(Breaks,f=.15)xaxt="n",yaxt'n",type="n")
              rect(0,min(Breaks),.25,max(Breaks))
              rect(0,Breaks[1:length(Breaks)-1],.25,Breaks[2:length(Breaks)],col=jet.colors(length(Breaks)-1),
                 border=jet.colors(length(Breaks)-1))
              Brks<-pretty(Breaks)
              Brks<-Brks[Brks>=min(Breaks) & Brks<max(Breaks)]
              segments(x0=.25,y0=Brks,x1=.3,y1=Brks)
              text(x=.4,y=Brks,labels=signif(exp(Brks),digits=3))

    dev.off()
    
#===================================
#Straight Ratio Here
#===================================
   png(file.path(OutputGraphics,paste(Vars[v],month.abb[m],"VicRawRatio.png",sep="")),width=1000,
                      height=700)
    MonthlyDiff<-(ImgLst[[1]][[m]]+2)/(combineMapLst[[m]]+2) #I'm adding the 1 to avoid division by zero as well as
    #Sgn<-sign(MonthlyDiff)
    #MonthlyDiff<-Sgn*exp(abs(MonthlyDiff))
    #small values leading to large differences
       # breakRng<-range(MonthlyDiff,na.rm=TRUE)
       #I think I want the break range consistent across all plots
         breakRng<-quantile(MonthlyDiff,na.rm=TRUE,probs=c(.02,.98))
         MonthlyDiff[MonthlyDiff<min(breakRng)]<-min(breakRng)
         MonthlyDiff[MonthlyDiff>max(breakRng)]<-max(breakRng)
         r<-max(abs(breakRng))
         Breaks<-c(rev(1/seq(from=1,to=max(c(breakRng,1/breakRng)),length=50)),
                   seq(from=1,to=max(c(breakRng,1/breakRng)),length=50)[2:50])
        par(mar=c(6,5,6,6))
           my.filled.contour(x=countryLon,y=countryLat,z=MonthlyDiff,zlim=c(-r,r),color.palette=jet.colors,
               plot.axes = {map("state",lwd=2,col="grey",add=TRUE) },
                 xlab="Longitude",
                 ylab="Latitude",main=paste(month.name[m],toupper(Vars[v]),"log((CMIP5 VIC + 2)/(CMIP3 VIC + 2))"),
                 cex.lab=2,cex.main=2)
          image.plot(MonthlyDiff,col=jet.colors(length(Breaks)-1),breaks=Breaks,zlim=breakRng)
    dev.off()
    
#=============================
#Difference Here
#=============================
png(file.path(OutputGraphics,paste(Vars[v],month.abb[m],"VicDiff.png",sep="")),width=1000,
                  height=700)
                  
MonthlyDiff<-ImgLst[[1]][[m]]-combineMapLst[[m]]
   # breakRng<-range(MonthlyDiff,na.rm=TRUE)
   #I think I want the break range consistent across all plots
     breakRng<-quantile(MonthlyDiff,na.rm=TRUE,probs=c(.02,.98))
     MonthlyDiff[MonthlyDiff<min(breakRng)]<-min(breakRng)
     MonthlyDiff[MonthlyDiff>max(breakRng)]<-max(breakRng)
     breakRng<-range(MonthlyDiff,na.rm=TRUE)
     r<-max(abs(breakRng))
     Breaks<-seq(from=-r,to=r,length=length(Colors)+1)

    par(mar=c(6,5,6,6))
     my.filled.contour(x=countryLon,y=countryLat,z=MonthlyDiff,zlim=range(Breaks),color.palette=jet.colors,
           plot.axes = {map("state",lwd=2,col="grey",add=TRUE) },
             xlab="Longitude",
             ylab="Latitude",main=paste(month.name[m],toupper(Vars[v]),"difference (CMIP5 VIC - CMIP3 VIC)"),
             cex.lab=2,cex.main=2)
       par(oma=c( 0,1,0,0))
         my.image.plot(MonthlyDiff,legend.only=TRUE,col=jet.colors(length(Breaks)-1),breaks=Breaks,zlim=range(Breaks),cex.axis=1.5)
dev.off()
#================================
# VIC 4.1.2 here
#================================
  Colors   = colorRampPalette(c("slategray1","royalblue1","blue4"))(50)
    jet.colors <- colorRampPalette(Colors)
png(file.path(OutputGraphics,paste(Vars[v],month.abb[m],"Vic421_CMIP5.png",sep="")),width=1000,
                  height=700)
                  
MonthlyDiff<-ImgLst[[1]][[m]]
 breakRng<-c(quantile(ImgLst[[1]][[m]],na.rm=TRUE,probs=c(.01,.99)),
             quantile(combineMapLst[[m]],na.rm=TRUE,probs=c(.01,.99)))
     MonthlyDiff[MonthlyDiff<min(breakRng)]<-min(breakRng)
     MonthlyDiff[MonthlyDiff>max(breakRng)]<-max(breakRng)
     breakRng<-range(MonthlyDiff,na.rm=TRUE)
     r<-max(abs(breakRng))
     Breaks<-seq(from=min(breakRng),to=max(breakRng),length=length(Colors)+1)
    par(mar=c(6,5,6,6))
     my.filled.contour(x=countryLon,y=countryLat,z=MonthlyDiff,zlim=range(Breaks),color.palette=jet.colors,
           plot.axes = {map("state",lwd=2,col="grey",add=TRUE) },
             xlab="Longitude",
             ylab="Latitude",main=paste(month.name[m],toupper(Vars[v]),"CMIP5 VIC 4.1.2"),
             cex.lab=2,cex.main=2)
       par(oma=c( 0,1,0,0))
         my.image.plot(MonthlyDiff,legend.only=TRUE,col=jet.colors(length(Breaks)-1),breaks=Breaks,zlim=range(Breaks),cex.axis=1.5)
dev.off()


                  
png(file.path(OutputGraphics,paste(Vars[v],month.abb[m],"Vic407_CMIP3.png",sep="")),width=1000,
                  height=700)

MonthlyDiff<-combineMapLst[[m]]
     MonthlyDiff[MonthlyDiff<min(breakRng)]<-min(breakRng)
     MonthlyDiff[MonthlyDiff>max(breakRng)]<-max(breakRng)
     breakRng<-range(MonthlyDiff,na.rm=TRUE)

    par(mar=c(6,5,6,6))
     my.filled.contour(x=countryLon,y=countryLat,z=MonthlyDiff,zlim=range(Breaks),color.palette=jet.colors,
           plot.axes = {map("state",lwd=2,col="grey",add=TRUE) },
             xlab="Longitude",
             ylab="Latitude",main=paste(month.name[m],toupper(Vars[v]),"CMIP3 VIC 4.0.7"),
             cex.lab=2,cex.main=2)
       par(oma=c( 0,1,0,0))
         my.image.plot(MonthlyDiff,legend.only=TRUE,col=jet.colors(length(Breaks)-1),breaks=Breaks,zlim=range(Breaks),cex.axis=1.5)
dev.off()
 #Sys.sleep(2)
}
}
