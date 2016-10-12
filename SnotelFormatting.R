library(fields)
library(ncdf4)
library(raster)
library(rgdal)
library(RColorBrewer)
library(viridis)
library(wesanderson)
library(ggplot2)
#this should run after NCDFVicsSatelliteToRasters.R which will read in the netCDFs

#this is shiny stack compareMapsCandidaData
load("C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\graphics\\ShinyDatNewNewStacks.RData")
source("C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\HydroCode\\ggplot2pairs.R")
SnotelPath<-"E:\\ClimateCache\\SnotelData"
fileList<-list.files(SnotelPath)
Errored<-grep("ERROR",fileList)
fileList<-fileList[-c(Errored)]
SnotelCoords<-read.csv("C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\SnotelLocs.csv")
cols<-c(wes_palettes$Cavalcanti[c(1,2,4,5)],wes_palettes$GrandBudapest2[1])

i=1

pdf("C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\graphics\\AllCurves.pdf")
par(mfrow=c(4,4),oma=c(0,1,5,0),mar=c(1,1,1,0))
#eventually we'll have a for loop here
count<-0 #count to tell me when to add a legend
annSums<-as.data.frame(matrix(NA,ncol=6,nrow=length(fileList)))
MonthlyByStation<-as.data.frame(matrix(NA,ncol=10,nrow=12*length(fileList)))
names(annSums)<-c("vic407","vic412","uw","satellite","SNOTEL","state")

for(i in 1:length(fileList)){
  Split<-strsplit(as.character(SnotelCoords$site_name[i])," ")[[1]]
  siteNumber<-Split[length(Split)]
  siteNumber<-substr(siteNumber,start=2,stop=(nchar(siteNumber)-1))
  prettyName<-paste(SnotelCoords$site_name[i] ,SnotelCoords$state[i])
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
      #snowDat<-snowDat[snowDat$year>=min(years) & snowDat$year<=max(years),]
      #find that lat and lon
      XYdat<-as.data.frame(cbind(X=SnotelCoords$lon[i],Y=SnotelCoords$lat[i]))
      v407<-extract(ShinyMapLst[[1]][[4]],XYdat)[1:12]
      v412<-extract(ShinyMapLst[[1]][[5]],XYdat)[1:12]
      uw<-extract(ShinyMapLst[[1]][[6]],XYdat)[1:12]
      satDat<-extract(ShinyMapLst[[1]][[7]],XYdat)[1:12]

      annSums[i,c(1,2,3,4)]<-c(sum(v407),sum(v412),sum(uw),sum(satDat))
      if(nrow(snowDat)>12){
        #Take the mean over the available years for snotel and then caculate the annual mean
        #converting units
         meanSWE<-aggregate(snowDat$swe,FUN=mean,list(month=snowDat$month))*25.4
         annSums[i,5]<-sum(meanSWE[,2])
      }
      annSums[i,6]<-substr(SiteName,start=1,stop=2)
      Ylim<-range(v407,v412,satDat,meanSWE,uw,na.rm=TRUE)

      plot(1:12,v407,type="l",ylim=Ylim,xaxt="n",xlab="",ylab="",lwd=2,bty="l",mgp=c(0,.2,0),main=SiteName,tck=-.01,col=cols[1])
      lines(1:12,v412,col=cols[2],lwd=2)
      lines(1:12,satDat,col=cols[3],lwd=2)
      lines(1:12,uw,col=cols[4],lwd=2)
      mtext("VIC 4.0.7, 4.1.2, Sattelite and Snotel data \ncompared over 1988-1999",side=3,outer=TRUE,cex=1.5,line=1)
      if(length(meanSWE$x)==12){
        lines(1:12,meanSWE$x,col=cols[5],lwd=2)
        MonthlyByStation[((i-1)*12+1:12),5] <- meanSWE$x
      }
      MonthlyByStation[((i-1)*12+1:12),1:4] <- cbind(v407,v412,satDat,uw)
      MonthlyByStation[((i-1)*12+1:12),6:10] <- cbind(rep(SiteName,times=12),1:12,
          rep(SnotelCoords$lon[i],times=12),rep(SnotelCoords$lat[i],times=12),rep(prettyName,times=12))
  }
  if(count==15){
  plot(0:1,0:1,type="n",xlab="",ylab="",xaxt="n",yaxt="n",bty="n")
  legend("topleft",c("VIC 4.0.7","VIC 4.1.2","Satellite","Univ Wash","SNOTEL"),fill=cols,cex=1.5)
  count<-0
  }
}

dev.off()

 names(MonthlyByStation)<-c("VIC407","VIC412","Sat","UW",
                            "SNOTEL","SiteName","Month","Lon","Lat",prettyName)
 MonthlyByStation<-na.omit(MonthlyByStation)
save(ShinyMapLst,MonthlyByStation,file=file.path(OutputGraphics,"ShinyDatStacksPrettyNames.RData"))
#============================================
# pairs plot
#a couple of the modeled have extremely high values 
d<-annSums
d[cbind(d[,c(1:5)]>10000,rep(FALSE,times=nrow(d)))]<-10000
d$state<-as.factor(d$state)

pdf(file.path(OutputGraphics,"ParisPlotLinesForStates.pdf"))
ggpairs(dat=d,alph=.2,pointSize=1.4,DevScore=1,showResp=FALSE)
dev.off()

#============================================
#looking at the relationships between errors lat, lon, elevation...
#I want to look at the Error by elevation and based on whether there was a snotel sight at the location
ras<-raster("C:\\Users\\mtalbert\\Desktop\\Climate\\InputLayers\\ElevationProj.tif")
Elev<-extract(ras,cbind(SnotelCoords$lon,SnotelCoords$lat))

#looks like the reported elevations agree with my elevation raster
plot(Elev,SnotelCoords$elev)
 cor(Elev,SnotelCoords$elev)
 ElevDat<-cbind(Elev,SnotelCoords$elev*.3048006)

coordgrid<-expand.grid(countryLon,countryLat)
countryElev<-extract(ras,coordgrid)
 #calculate the index of the closest lat and closest lon to any point
 fun<-function(x,y){abs(x-y)}
 closeLons<-outer(SnotelCoords$lon,countryLon,fun)
 closeLons<-apply(closeLons,1,which.min)
 closeLats<-outer(SnotelCoords$lat,countryLat,fun)
 closeLats<-apply(closeLats,1,which.min)
 #calculate distance from any location to the nearest snotel site
 MinDistToSnotel<-rep(5000,times=nrow(coordgrid))
 SnotelIndx<-vector()
 for(i in 1:nrow(SnotelCoords)){
   LonMatch<-which((coordgrid[,1]-countryLon[closeLons[i]])==0,arr.ind=TRUE)
   LatMatch<-which((coordgrid[,2]-countryLat[closeLats[i]])==0,arr.ind=TRUE)
   #record the index in the grid of the closest match to a snotel
   SnotelIndx[i]<-intersect(LonMatch,LatMatch)
   MinDistToSnotel<-apply(cbind(MinDistToSnotel,
                        sqrt((coordgrid[,1]-countryLon[closeLons[i]])^2 +
                        (coordgrid[,2]-countryLat[closeLats[i]])^2)),1,min)
  print(mean(MinDistToSnotel))
 }
 #plotting to make sure I've done it right
  DistImg<-matrix(MinDistToSnotel,nrow=length(countryLon))
  image.plot(DistImg,y=countryLat,x=countryLon,xlim=c(-120,-100),zlim=c(0,10))
  points(x=SnotelCoords$lon,y=SnotelCoords$lat)
  
#I really thought there was a relationship here but maybe not...
  vic412SWE<-extract(ShinyMapLst[[1]][[5]][[13]],coordgrid)
  vic407SWE<-extract(ShinyMapLst[[1]][[4]][[13]],coordgrid)
  
  vic412SWEfeb<-extract(ShinyMapLst[[1]][[5]][[2]],coordgrid)
  vic407SWEfeb<-extract(ShinyMapLst[[1]][[4]][[2]],coordgrid)
    AnnualSum<-as.vector(log((vic412SWE+25)/(vic407SWE+25)))
    
    #Febuary was the worst month so use that
    MonthlyImg<-log((vic412SWEfeb+10)/(vic407SWEfeb+10))
    MonthlyDiff<-as.vector(log((vic412SWEfeb+10)/(vic407SWEfeb+10)))

     ErrorByElev<-data.frame(Elev=countryElev,FebError=MonthlyDiff,AnnualError=AnnualSum,
       AbsAnnError=abs(AnnualSum),absFebError=abs(MonthlyDiff),Lon=coordgrid[,1],Lat=coordgrid[,2],DistToSnotel=MinDistToSnotel)
      ErrorByElev$Snotel<-rep("Not a SNOTEL site",times=nrow(ErrorByElev))
     ErrorByElev$Snotel[SnotelIndx]<-"SNOTEL site"
     #making sure all my data line up
     for(i in 1:7){
        DistImg<-matrix(ErrorByElev[,i],nrow=length(countryLon))
          image.plot(DistImg,y=countryLat,x=countryLon)
          Sys.sleep(4)
      }
      ErrorByElev<-ErrorByElev[vic412SWE!=0 & vic407SWE!=0,]
     ErrorByElev<-ErrorByElev[complete.cases(ErrorByElev),]

     ggpairs(dat=ErrorByElev,alph=.01,pointSize=.1,DevScore=1,showResp=FALSE)
     #there are only errors where it snows so here I removed nonsnowy areas
     #but there are some bad error down south in the mountains as well
     #ErrorByElev<-ErrorByElev[ErrorByElev$Lat>37,]
     annSums<-annSums[complete.cases(annSums),]
     d<-data.frame(SNOTEL=c(annSums$SNOTEL,annSums$SNOTEL),
              VIC=c(annSums$vic407,annSums$vic412),
              Model=factor(c(rep("VIC 4.0.7",times=nrow(annSums)),
                              rep("VIC 4.1.2",times=nrow(annSums)))))

     png(file.path(OutputGraphics,"SnotelvsVICmodels.png"),width=900,
                      height=900,type="cairo")
     ggplot(d,aes(x=SNOTEL,y=VIC,color=Model))+geom_smooth()+geom_abline(intercept=0,slope=1)+
          coord_cartesian(ylim=c(0,20000),xlim=c(0,20000))+
          theme(axis.text=element_text(size=18),
                axis.title=element_text(size=20,face="bold"),
                plot.title=element_text(size=20,face="bold"),
                legend.title=element_text(size=24),
                legend.text=element_text(size=18))+
                ggtitle("Sum of annual VIC model output extracted from SNOTEL\nlocations averaged over 1988-1999")
     dev.off()
    #======================================
    # looking at what's causing the error with interactions
    ggplot(ErrorByElev,aes(x=Elev,y=exp(AbsAnnError),color=Snotel))+geom_point(size=.1,
    alpha=.1)+geom_smooth(size=1.2)+
       scale_color_manual(values=c("lightsteelblue","blue4"))+ylim(0,quantile(ErrorByElev$Error,.97))+
         theme_bw()+coord_cartesian(ylim=c(1,3))+
         theme(
          plot.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank())+xlab("Elevation")+ylab("exp(Absolute Annual Error)")
         theme(axis.line = element_line(color = 'black'))
         
    ggplot(ErrorByElev,aes(x=Elev,y=exp(AbsAnnError),color=Lat))+geom_point(size=1.2,alpha=.1)+geom_smooth()+
       scale_color_gradientn(colors=rev(viridis(20)))+
       theme_bw()+coord_cartesian(ylim=c(1,3))+   #coord_cartesian(ylim(as.vector(c(0,quantile(exp(ErrorByElev$AbsAnnError),.97)))))
         theme(
          plot.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank())+
         theme(axis.line = element_line(color = 'black'))
         
    ggplot(ErrorByElev,aes(x=Lat,y=exp(AbsAnnError),color=Elev))+geom_point(size=1,alpha=.2)+geom_smooth()+
         scale_color_gradientn(colors=rev(viridis(20)))+coord_cartesian(ylim=c(1,3))+theme_bw()+
         theme(
          plot.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank())+
         theme(axis.line = element_line(color = 'black'))


         
    ggplot(ErrorByElev,aes(x=Lat,y=exp(AbsAnnError),color=Snotel))+geom_point(size=.3,alpha=.2)+geom_smooth(size=1.2)+
       scale_color_manual(values=c("lightsteelblue","blue4"))+coord_cartesian(ylim=c(1,3))+theme_bw()+
         theme(
          plot.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank())+
         theme(axis.line = element_line(color = 'black'))

RFFit <- randomForest(AnnualError ~ Elev+Lat+Lon+DistToSnotel, data=ErrorByElev, mtry=3,
importance=TRUE,nodesize=100)
dat<-ErrorByElev[,c(1,6,7,8)]
 mins  <- sapply(dat, min,  na.rm=TRUE)
         maxs  <- sapply(dat, max,  na.rm=TRUE)
         means <- sapply(dat, mean, na.rm=TRUE)
         n <- 100
     par(mfrow=c(2,2),oma=c(0,0,0,0),mar=c(4,4,0,0),mgp=c(2,.5,0))
     for(pIdx in 1:ncol(dat)){
      test <- do.call("rbind", replicate(n, means, simplify=FALSE))
                              test[,pIdx] <- seq(mins[pIdx], maxs[pIdx], length.out=n)
      resp<-predict(RFFit,test)
      plot(x=test[,pIdx],y=resp,type="l",ylab="response",xlab=names(dat)[pIdx],bty="l",ylim=range(ErrorByElev$AnnualError))
       }
       
    ggplot(ErrorByElev,aes(x=Elev,y=Lat,color=AbsAnnError))+geom_point(size=1,alpha=.5)+
      scale_color_gradientn(colors=rev(viridis(20)))+ facet_grid(~SnotelLoc)
    ggplot(ErrorByElev,aes(x=DistToSnotel,y=Error,color=Elev))+geom_point(size=1,alpha=.1)+
      geom_smooth(size=1.2)+
        scale_color_gradientn(colors=rev(viridis(20)))
    ggplot(ErrorByElev,aes(x=Elev,y=DistToSnotel,color=Error))+geom_point(size=1,alpha=.1)+
      scale_color_gradientn(colors=rev(viridis(20)))


d<-data.frame(SNOTEL=c(annSums$SNOTEL,annSums$SNOTEL),
              VIC=c(annSums$vic407,annSums$vic412),
              Model=factor(c(rep("VIC 4.0.7",times=nrow(annSums)),
                              rep("VIC 4.1.2",times=nrow(annSums)))))

ggplot(d,aes(x=SNOTEL,y=VIC,color=Model))+geom_smooth()+geom_abline(intercept=0,slope=1)+
          coord_cartesian(ylim=(c(2000,13000)))+coord_cartesian(xlim=(c(2000,13000)))



