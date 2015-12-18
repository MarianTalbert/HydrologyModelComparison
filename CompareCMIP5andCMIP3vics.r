 library(ncdf4)
 Paths=c(cmip5Obs = "C:\\Users\\mtalbert\\Downloads\\conus_c5.para_v0.monthly.swe.",
         cmip3Obs = "C:\\Users\\mtalbert\\Downloads\\obs.maurer_2002.mo.monthly.swe.")
 latName<-c("latitude","lat")
 lonName<-c("longitude","lon")        
 years<-1970:1999
 Ts<-vector()
 Year<-vector()
 Month<-vector()
 Model<-vector()
 for(p in 1:length(Paths)){
     for(yr in years){
     filePath<-paste(Paths[p],yr,".nc",sep="") 
     Clim<-nc_open(filePath)
      ex<-extent(Boundary)
         if(yr==years[1]){ #do this for the first year only because it takes a bit to caluclate
           Lat <- ncvar_get(Clim,latName[p])
           Lon <- ncvar_get(Clim,lonName[p])
            
           LatInBox<-which(Lat>=ex@ymin & Lat<=ex@ymax,arr.ind=TRUE)
           LonInBox<-which(Lon>=ex@xmin & Lon<=ex@xmax,arr.ind=TRUE)
           
           ind<-expand.grid(seq(from=min(LonInBox),to=max(LonInBox)),seq(from=min(LatInBox),to=max(LatInBox)))
         }
         RastArray<-ncvar_get(Clim, varid="swe", start=c(min(ind[,1]),min(ind[,2]),1), 
                     count=c((max(ind[,1])-min(ind[,1])+1),(max(ind[,2])-min(ind[,2])+1),12))
         
         #average over space and add to the time series           
        Ts<-c(Ts,apply(RastArray,3,mean,na.rm=TRUE))
        Year<-c(Year,rep(yr,times=12))
        Month<-c(Month,1:12) 
        Model<-c(Model,rep(names(Paths),times=12)) 
        nc_close(Clim)
    }
 }  
 dat<-data.frame(Time=Year+Month/12,Month=Month,Year=Year,Model=factor(Model),Ts=Ts)
  ggplot(dat, aes(x = Time, y = Ts,group=Model)) + geom_line(aes(colour = Model))
  #subsetting to winter
  ggplot(dat[dat$Month%in%c(1,2,3,4,12),], aes(x = Time, y = Ts,group=Model)) + geom_line(aes(colour = Model))
  
  #and looking at monthy... looks like an off by one issue   
   ggplot(dat, aes(x = Month, y = Ts,Colour=Year,Group=Year)) +geom_point(Colour=Year)+ 
   facet_grid(Model ~ .)
  
   cor(dat$Ts[dat$Model==names(Paths)[1]],dat$Ts[dat$Model==names(Paths)[2]])
   cmip3dat<-dat$Ts[dat$Model==names(Paths)[2]]
   cmip5dat<-dat$Ts[dat$Model==names(Paths)[1]]
   cor(cmip3dat[-length(cmip3dat)],cmip5dat[-1])