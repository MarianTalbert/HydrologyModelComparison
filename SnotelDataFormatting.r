library(wesanderson)
#this should run after compareMapsCandidaData which will read in the netCDFs
SnotelPath<-"E:\\ClimateCache\\SnotelData"
fileList<-list.files(SnotelPath)
Errored<-grep("ERROR",fileList)
fileList<-fileList[-c(Errored)]
SnotelCoords<-read.csv("C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\SnotelLocs.csv")
cols<-wes_palettes$Cavalcanti[c(1,2,4,5)]

i=1

pdf("C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\graphics\\AllCurves.pdf")
par(mfrow=c(4,4),oma=c(0,1,5,0),mar=c(1,1,1,0))
#eventually we'll have a for loop here
count<-0 #count to tell me when to add a legend
annSums<-as.data.frame(matrix(NA,ncol=5,nrow=length(fileList)))
names(annSums)<-c("vic407","vic412","satellite","SNOTEL","state")

for(i in 1:length(fileList)){
  Split<-strsplit(as.character(SnotelCoords$site_name[i])," ")[[1]]
  siteNumber<-Split[length(Split)]
  siteNumber<-substr(siteNumber,start=2,stop=(nchar(siteNumber)-1))

  SiteName<-paste(SnotelCoords$state[i],siteNumber,sep="_")

  fileToUse<-match(SiteName,gsub(".csv","",fileList))
  if(!is.na(fileToUse)){
  count<-count+1
      snowDat<-read.csv(file.path(SnotelPath,fileList[fileToUse]))
      day<-as.numeric(substr(snowDat$date2,9,10))
      snowDat<-snowDat[day==1,]
      year<-as.numeric(substr(snowDat$date2,1,4))
      month<-as.numeric(substr(snowDat$date2,6,7))
      snowDat<-data.frame(swe=snowDat$SWE,year=year,month=month)
      #keeping all the data in the same year range
      snowDat<-snowDat[snowDat$year>=min(years) & snowDat$year<=max(years),]
      #find that lat and lon
      lonInd<-which.min(abs(SnotelCoords$lon[i]-countryLon))
      latInd<-which.min(abs(SnotelCoords$lat[i]-countryLat))
      v407<-unlist(lapply(vic407,"[",lonInd,latInd))
      v412<-unlist(lapply(vic412,"[",lonInd,latInd))

      #of course the satelite data is different
      lonInd<-which.min(abs(SnotelCoords$lon[i]-satLon))
      latInd<-which.min(abs(SnotelCoords$lat[i]-satLat))
      satDat<-unlist(lapply(monthlySatellite,"[",lonInd,latInd))
      annSums[i,c(1,2,3)]<-c(sum(v407),sum(v412),sum(satDat))
      if(nrow(snowDat)>12){
         meanSWE<-aggregate(snowDat$swe,FUN=mean,list(month=snowDat$month))*25.4
         annSums[i,4]<-sum(meanSWE)
      }
      annSums[i,5]<-substr(SiteName,start=1,stop=2)
      Ylim<-range(v407,v412,satDat,meanSWE,na.rm=TRUE)

      plot(1:12,v407,type="l",ylim=Ylim,xaxt="n",xlab="",ylab="",lwd=2,bty="l",mgp=c(0,.2,0),main=SiteName,tck=-.01,col=cols[1])
      lines(1:12,v412,col=cols[2],lwd=2)
      lines(1:12,satDat,col=cols[3],lwd=2)
      mtext("VIC 4.0.7, 4.1.2, Sattelite and Snotel data \ncompared over 1988-1999",side=3,outer=TRUE,cex=1.5,line=1)
      if(length(meanSWE$x)==12) lines(1:12,meanSWE$x,col=cols[4],lwd=2)
  }
  if(count==15){
  plot(0:1,0:1,type="n",xlab="",ylab="",xaxt="n",yaxt="n",bty="n")
  legend("topleft",c("VIC 4.0.7","VIC 4.1.2","Satellite","SNOTEL"),fill=cols,cex=1.5)
  count<-0
  }
}
dev.off()

d<-annSums[complete.cases(annSums),]
d[cbind(d[,c(1:4)]>10000,rep(FALSE,times=nrow(d)))]<-10000
d$state<-as.factor(d$state)


ggpairs(dat=d,alph=.2,pointSize=1.4,DevScore=1,showResp=FALSE)

#I want to look at the Error by elevation and based on whether there was a snotel sight at the location
ras<-raster("C:\\Users\\mtalbert\\Desktop\\Climate\\InputLayers\\ElevationProj.tif")
Elev<-extract(ras,cbind(SnotelCoords$lon,SnotelCoords$lat))
plot(Elev,SnotelCoords$elev)
 cor(Elev,SnotelCoords$elev)
 ElevDat<-cbind(Elev,SnotelCoords$elev*.3048006)


coordgrid<-expand.grid(countryLon,countryLat)
countryElev<-extract(ras,coordgrid)
 fun<-function(x,y){abs(x-y)}
 closeLons<-outer(SnotelCoords$lon,countryLon,fun)
 closeLons<-apply(closeLons,1,which.min)
 closeLats<-outer(SnotelCoords$lat,countryLat,fun)
 closeLats<-apply(closeLats,1,which.min)
 MinDistToSnotel<-rep(5000,times=nrow(coordgrid))
 SnotelIndx<-vector()
 for(i in 1:nrow(SnotelCoords)){
   LonMatch<-which((coordgrid[,1]-countryLon[closeLons[i]])==0,arr.ind=TRUE)
   LatMatch<-which((coordgrid[,2]-countryLat[closeLats[i]])==0,arr.ind=TRUE)
   SnotelIndx[i]<-intersect(LonMatch,LatMatch)
   MinDistToSnotel<-apply(cbind(MinDistToSnotel,
                        sqrt((coordgrid[,1]-countryLon[closeLons[i]])^2 +
                        (coordgrid[,2]-countryLat[closeLats[i]])^2)),1,min)
  print(i)
 }
 
#I really thought there was a relationship here but maybe not...
for(m in c(1,2,3,4,11,12)){
    png(file.path(OutputGraphics,paste(month.abb[m],"Snotel",".png",sep="")),height=1000,width=750)
    MonthlyImg<-log((vic412[[m]]+4)/(vic407[[m]]+4))
    MonthlyDiff<-as.vector(abs(log((vic412[[m]]+4)/(vic407[[m]]+4))))

     ErrorByElev<-data.frame(Elev=countryElev,Error=MonthlyDiff,Lon=coordgrid[,1],Lat=coordgrid[,2],DistToSnotel=MinDistToSnotel)
      ErrorByElev$SnotelLoc<-rep(FALSE,times=nrow(ErrorByElev))
     ErrorByElev$SnotelLoc[SnotelIndx]<-TRUE
     ErrorByElev<-ErrorByElev[complete.cases(ErrorByElev),]
     ErrorByElev<-ErrorByElev[ErrorByElev$Lat>37,]

    p1<-ggplot(ErrorByElev,aes(x=Elev,y=Error,color=SnotelLoc))+geom_point(size=.3,alpha=.1)+geom_smooth(size=1.2)+
       scale_color_manual(values=c("thistle4","blue4"))+ylim(0,quantile(ErrorByElev$Error,.97))
    p2<-ggplot(ErrorByElev,aes(x=Elev,y=Error,color=Lat))+geom_point(size=1,alpha=.2)+geom_smooth()+
       scale_color_gradientn(colors=rev(viridis(20)))+
       ylim(0,quantile(ErrorByElev$Error,.97))
    p3<-ggplot(ErrorByElev,aes(x=Lat,y=Error,color=Elev))+geom_point(size=1,alpha=.2)+geom_smooth()+
         scale_color_gradientn(colors=rev(viridis(20)))+ylim(0,quantile(ErrorByElev$Error,.97))
    p4<-ggplot(ErrorByElev,aes(x=Lat,y=Error,color=SnotelLoc))+geom_point(size=.3,alpha=.2)+geom_smooth(size=1.2)+
       scale_color_manual(values=c("thistle4","blue4"))+ylim(0,quantile(ErrorByElev$Error,.97))
    p5<-ggplot(ErrorByElev,aes(x=Elev,y=Lat,color=Error))+geom_point(size=1,alpha=.5)+scale_color_gradientn(colors=rev(viridis(20)))+ facet_grid(~SnotelLoc)
    p6<-ggplot(ErrorByElev,aes(x=DistToSnotel,y=Error,color=Elev))+geom_point(size=1,alpha=.1)+geom_smooth(size=1.2)+
        scale_color_gradientn(colors=rev(viridis(20)))
    p7<-ggplot(ErrorByElev,aes(x=Elev,y=DistToSnotel,color=Error))+geom_point(size=1,alpha=.1)+scale_color_gradientn(colors=rev(viridis(20)))
    multiplot(p1,p2,p3,p4,p5,p6,p7,cols=3)
    dev.off()
    Sys.sleep(5)
}


