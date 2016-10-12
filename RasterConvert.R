convertToRaster<-function(Map,Lon,Lat,RastType="GTiff",OutputPath,fileName){
 #just a simple function to convert a netCDF to a raster
 #MapClass=MappedClass object
 #RastType = format options from writeRaster
 #fileName = no file extension write raster figures it out
    Map<-t(Map)
    Map<-Map[nrow(Map):1,]
    r<-raster(Map)
    projection(r)<-
       CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    xmin(r) <- min(Lon)
    xmax(r) <- max(Lon)
    ymin(r) <- min(Lat)
    ymax(r) <- max(Lat)
    
    if(!missing(OutputPath)) writeRaster(r,file.path(OutputPath,fileName),format=RastType,overwrite=TRUE)
    else return(r)
}
