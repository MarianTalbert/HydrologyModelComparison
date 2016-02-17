
myImagePlot<-function(Image,VarName,Title,fileName,m,breakRng,Breaks,Lat,Lon,OutputGraphics,Colors,SnowSites){
   jet.colors <- colorRampPalette(Colors)
  png(file.path(OutputGraphics,paste(month.abb[m],VarName,fileName,".png",sep="")),width=1000,
                      height=700)
         Image[Image<min(breakRng)]<-min(breakRng)
         Image[Image>max(breakRng)]<-max(breakRng)

    par(mar=c(6,5,6,6))
         my.filled.contour(x=countryLon,y=countryLat,z=Image,zlim=c(min(breakRng),max(breakRng)),color.palette=jet.colors,
               plot.axes = {map("state",lwd=2,col="grey",add=TRUE) },
                 xlab="Longitude",
                 ylab="Latitude",main=paste(month.name[m],toupper(VarName),Title),
                 cex.lab=2,cex.main=2)
         points(SnowSites$lon,SnowSites$lat,cex=.5,pch=16)        
           par(oma=c( 0,1,0,0))
             my.image.plot(Image,legend.only=TRUE,col=jet.colors(length(Breaks)-1),breaks=Breaks,zlim=c(min(breakRng),max(breakRng)),cex.axis=1.5)
    dev.off()
    }