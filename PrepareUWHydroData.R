Clim<-nc_open("E:\\ClimateCache\\UWHydrology\\SWE.monthly.1950-2000.nc")
lon<-ncvar_get(Clim,"lon") #this is 464 unique
lat<-ncvar_get(Clim,"lat") #this is 224 unique
SWE<-ncvar_get(Clim,"SWE",start=c(1,1),count=c(76576,1))
Time<-ncvar_get(Clim,"timestep")
uLon<-sort(unique(lon))
uLat<-sort(unique(lat),decreasing=TRUE)
UWhydro<-list()
for(month in 1:12){
  TimeIndx<-seq(from=month,to=length(Time),by=12)
  outDat<-matrix(0,nrow=224,ncol=464)
  for(year in TimeIndx){
    SWE<-ncvar_get(Clim,"SWE",start=c(1,year),count=c(76576,1))
      m<-matrix(data=NA,nrow=224,ncol=464)
      #loop over all of the indicies and find the correct
      #lat and lon
      for(i in 1:length(SWE)){
          latInd<-which(uLat==lat[i],arr.ind=TRUE)
          lonInd<-which(uLon==lon[i],arr.ind=TRUE)
          m[latInd,lonInd]<-SWE[i]
      }
      outDat<-outDat+m
  }
  print(month)
  UWhydro[[month]]<-outDat/length(TimeIndx)
}
r<-t(UWhydro[[1]])
 UWhydroStk<-stack(lapply(UWhydro,FUN=raster,xmn=min(uLon),xmx=max(uLon),ymn=min(uLat),ymx=max(uLat),
      crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))

 UWhydroStk<-addLayer(UWhydroStk,raster(Reduce("+",UWhydro),
       xmn=min(uLon),xmx=max(uLon),ymn=min(uLat),ymx=max(uLat),
       crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))
ShinyMapLst[[1]]$UWhydro<-UWhydroStk
ShinyMapLst[[1]]$Satellite<-Satellite
save(ShinyMapLst,countryLat,countryLon,file=file.path(OutputGraphics,"ShinyDatNewNewStacks.RData"))

#ShinyMapLst[[1]][[6]]<-UWhydro

