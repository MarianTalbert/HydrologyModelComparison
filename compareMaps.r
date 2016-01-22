library(fields)
library(ncdf4)
OutputGraphics<-"C:\\Users\\mtalbert\\Desktop\\Climate\\ParkOutput\\HazenND\\graphics"
latName <- c("latitude",rep("lat",times=3))
lonName <- c("longitude",rep("lon",times=3))        
years <- 1970:1999
ImgLst <- list() 
Paths=c(cmip5Obs = "C:\\Users\\mtalbert\\Downloads\\conus_c5.para_v0.monthly.swe.",
         cmip3mo = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\mo\\obs.maurer_2002.mo.monthly.swe.",
         cmip3gbas = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\gbas\\obs.maurer_2002.gbas.monthly.swe.",
         cmip3colo = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\colo\\obs.maurer_2002.colo.monthly.swe.")        
 for(p in 1:length(Paths)){
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
               RastArray <- ncvar_get(Clim, varid="swe", start=c(1,1,m), 
                           count=c(length(Lon),length(Lat),1))
               
               #average over space and add to the time series           
                OutputImg <- OutputImg+RastArray/length(years)
               nc_close(Clim)
        }
        MonthlyImages[[m]] <- OutputImg
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
d<-array(data=NA,c(length(countryLon),length(countryLat),3))
 #put the three regions in my array
 d[,,1]<-ImgLst[[2]][[m]]
 d[,,2]<-ImgLst[[3]][[m]]
 d[,,3]<-ImgLst[[4]][[m]]
combineMap<-apply(d,c(1,2),sum,na.rm=TRUE) 
f<-function(x){sum(is.na(x))==3}
NAmask<-apply(d,c(1,2),FUN=f)
NAmask[NAmask==TRUE]<-NA
combineMapLst[[m]]<-combineMap+NAmask 
}

Colors   = c(colorRampPalette(c("navy", "white"))(50),rev(colorRampPalette(c("red4", "white"))(50)))
jet.colors <- colorRampPalette(Colors)

for(m in 1:12){ 
png(file.path(OutputGraphics,paste(month.abb[m],"VicRatio.png",sep="")),width=1000,
                  height=700)
MonthlyDiff<-ImgLst[[1]][[m]]/combineMapLst[[m]]
   # breakRng<-range(MonthlyDiff,na.rm=TRUE)
   #I think I want the break range consistent across all plots
     breakRng<-c(0,2)
     #r<-max(abs(breakRng))
     # Breaks<-seq(from=-r,to=r,length=length(Colors)+1)
     Breaks<-seq(from=0,to=2,length=length(Colors)+1)
    par(mar=c(6,5,6,4))
     my.filled.contour(x=countryLon,y=countryLat,z=MonthlyDiff,zlim=range(Breaks),color.palette=jet.colors,
           plot.axes = {map("state",lwd=2,col="grey",add=TRUE) },
             xlab="Longitude",
             ylab="Latitude",main=paste(month.name[m],"ratio (CMIP5 VIC/CMIP3 VIC)"),
             cex.lab=2,cex.main=2) 
       par(oma=c( 0,1,0,0))
         my.image.plot(MonthlyDiff,legend.only=TRUE,col=jet.colors(length(Breaks)-1),breaks=Breaks,zlim=range(Breaks),cex.axis=2)       
dev.off()
 Sys.sleep(2)
             }

Rng<-c(0,30)
  my.filled.contour(x=countryLon,y=countryLat,z=ImgLst[[1]][[m]],zlim=Rng,color.palette=jet.colors,
           plot.axes = {map("state",lwd=2,col="grey",add=TRUE) })
 my.image.plot(MonthlyDiff,legend.only=TRUE,col=jet.colors(length(Breaks)-1),breaks=seq(from=min(Rng),to=max(Rng),length=length(Colors)+1),
   zlim=Rng,cex.axis=2)
   
Rng<-c(0,30)
  my.filled.contour(x=countryLon,y=countryLat,z=combineMapLst[[m]],zlim=Rng,color.palette=jet.colors,
           plot.axes = {map("state",lwd=2,col="grey",add=TRUE) })
 my.image.plot(MonthlyDiff,legend.only=TRUE,col=jet.colors(length(Breaks)-1),breaks=seq(from=min(Rng),to=max(Rng),length=length(Colors)+1),
   zlim=Rng,cex.axis=2)