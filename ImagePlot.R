myImagePlot<-function(Image,VarName,Title,fileName,m,breakRng,Breaks,Lat,Lon,OutputGraphics,Colors,SnowSites,LogToAbs=FALSE){
   jet.colors <- colorRampPalette(Colors)

   png(file.path(OutputGraphics,paste(month.abb[m],VarName,fileName,".png",sep="")),width=1300,
                      height=900,type="cairo")

         Image[Image<min(breakRng)]<-min(breakRng)
         Image[Image>max(breakRng)]<-max(breakRng)

    par(mar=c(6,5,6,6))
    if(LogToAbs){
       layout(matrix(c(1,2), 1, 2),c(10,1))
       par(oma=c(0,1,2,0),mar=c(6,5,4,0))
     }
         my.filled.contour(x=countryLon,y=countryLat,z=Image,zlim=c(min(breakRng),max(breakRng)),color.palette=jet.colors,
               plot.axes = {map("state",lwd=2,col="grey",add=TRUE) },
                 xlab="Longitude",
                 ylab="Latitude",main=paste(month.name[m],toupper(VarName),Title),
                 cex.lab=2.2,cex.main=2.3)
                 if(!missing(SnowSites)) points(SnowSites$lon,SnowSites$lat,cex=.5,pch=16)
                if(LogToAbs){
                par(mar=c(0,2,0,0))
                plot(c(0,.5),extendrange(Breaks,f=.15),xaxt="n",yaxt="n",type="n",bty="n",xlab="",ylab="")
                  rect(.05,min(Breaks),.15,max(Breaks))
                  rect(.05,Breaks[1:length(Breaks)-1],.15,Breaks[2:length(Breaks)],col=jet.colors(length(Breaks)-1),
                     border=jet.colors(length(Breaks)-1))
                  Brks<-c(log(5),log(4),log(3),log(2),log(1),log(1/2),log(1/3),log(1/4),log(1/5))
                  Brks<-Brks[Brks>=min(Breaks) & Brks<max(Breaks)]
                  segments(x0=.15,y0=Brks,x1=.2,y1=Brks)
                  text(x=.3,y=Brks,labels=signif(c(-1,1)[factor(I(Brks>=0))]*exp(abs(Brks)),digits=3),cex=1.4)
              }
             else{
           par(mar=c(6,5,6,6))
             my.image.plot(Image,legend.only=TRUE,col=jet.colors(length(Breaks)-1),breaks=Breaks,zlim=c(min(breakRng),max(breakRng)),
               cex.axis=1.5)
             }
    dev.off()
    }