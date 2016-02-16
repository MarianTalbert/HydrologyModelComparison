CompareVics<-function(ex,Paths){
 latName<-c("latitude","lat")
 lonName<-c("longitude","lon")        
 years<-1950:1999
 SWE<-vector()
 Year<-vector()
 Month<-vector()
 Model<-vector()
 for(p in 1:length(Paths)){
     for(yr in years){
     filePath<-paste(Paths[p],yr,".nc",sep="") 
     Clim<-nc_open(filePath)

         if(yr==years[1]){ #do this for the first year only because it takes a bit to caluclate
           Lat <- ncvar_get(Clim,latName[p])
           Lon <- ncvar_get(Clim,lonName[p])
            
           LatInBox<-which(Lat>=min(ex[2,]) & Lat<=max(ex[2,]),arr.ind=TRUE)
           LonInBox<-which(Lon>=min(ex[1,]) & Lon<=max(ex[1,]),arr.ind=TRUE)
           
           ind<-expand.grid(seq(from=min(LonInBox),to=max(LonInBox)),seq(from=min(LatInBox),to=max(LatInBox)))
         }
         RastArray<-ncvar_get(Clim, varid="swe", start=c(min(ind[,1]),min(ind[,2]),1), 
                     count=c((max(ind[,1])-min(ind[,1])+1),(max(ind[,2])-min(ind[,2])+1),12))
         
         #average over space and add to the time series           
        SWE<-c(SWE,apply(RastArray,3,mean,na.rm=TRUE))
        Year<-c(Year,rep(yr,times=12))
        Month<-c(Month,1:12) 
        Model<-c(Model,rep(names(Paths)[p],times=12))
        nc_close(Clim)
    }
 }  
 dat<-data.frame(Time=Year+Month/12,Month=Month,Year=Year,Model=factor(Model),SWE=SWE)
 grid.newpage()
  pushViewport(viewport(layout=grid.layout(2,2)))
  vplayout<- function(x,y)
	viewport(layout.pos.row=x, layout.pos.col=y)
	
  print(ggplot(dat, aes(x = Time, y = SWE,group=Model)) + geom_line(aes(colour = Model),size=1)+ ggtitle("Yearly Time Series"),vp=vplayout(1,2))
  #subsetting to winter
 # print(ggplot(dat[dat$Month%in%c(1,2,3,4,12),], aes(x = Time, y = SWE,group=Model)) + geom_line(aes(colour = Model))+ggtitle("Winter Subset"),vp=vplayout(1,2))

  #and looking at monthy... looks like an off by one issue   
   print(ggplot(dat, aes(x = Month, y = SWE,group=factor(Year),colour=factor(Year))) +geom_line(group=factor(Year))+
   facet_grid(Model ~ .)+stat_summary(aes(y = SWE,group=Model), fun.y=mean,geom="line",size=1.5,colour="red")+
   ggtitle("Monthly curves")+ theme(legend.position = 'none'),vp=vplayout(1:2,1))

   us <- map_data("state")
   gg <- ggplot(data=us)
gg <- gg + geom_map(data=us, map=us,
                    aes(x=long, y=lat, map_id=region,group=group),
                    fill="#ffffff", color="#7f7f7f", size=0.15) + coord_map("albers", lat0 = 39, lat1 = 45)
pointDat<-data.frame(long=rep(ex[1,],each=2),lat=c(rev(ex[2,]),ex[2,]))
gg<-gg+geom_polygon(data = pointDat, aes(x = long, y = lat), color = "red",fill="lightgrey")
print(gg,vp=vplayout(2,2))
cor(dat$SWE[dat$Model==names(Paths)[1]],dat$SWE[dat$Model==names(Paths)[2]])
}