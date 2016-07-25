 #some plots of the errors 
 #======================================
    # looking at what's causing the error with interactions
    # exp(AnnualError)==1 is a match between the two
    #added 25 to annual 
    
 load("C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\graphics/ggplotExplore")
 library(ggplot2)
 source("C:\\GoogleDrive\\Climate\\Rcode\\changeAlpha.R")
 OutputGraphics<-"C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\graphics"
     ggplot(ErrorByElev,aes(x=DistToSnotel,y=AnnualError,color=Snotel))+geom_point(size=.1,
    alpha=.1)+geom_smooth(size=1.2)+
       scale_color_manual(values=c("lightsteelblue","blue4"))+
         theme_bw()+coord_cartesian(ylim=c(.25,1.77))+
         theme(axis.text=element_text(size=18),
                axis.title=element_text(size=20,face="bold"),
                plot.title=element_text(size=20,face="bold"),
                legend.title=element_text(size=24),
                legend.text=element_text(size=18),
          plot.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank())+xlab("Elevation")+ylab("exp(Absolute Annual Error)")+
         theme(axis.line = element_line(color = 'black'))

    png(file.path(OutputGraphics,"ErrorByElevandLatAbs.png"), width=8, height=6, res=400, units="in")
    ggplot(ErrorByElev,aes(x=Lat,y=Elev,color=abs(AnnualError)))+geom_point(size=2.5,
    alpha=.2)+scale_color_gradientn(colors=rev(inferno(20)),name="abs(Annual \nError)")+
         theme_bw()+ theme(axis.text=element_text(size=12),
                axis.title=element_text(size=15,face="bold"),
                plot.title=element_text(size=15,face="bold"),
                legend.title=element_text(size=10),
                legend.text=element_text(size=10),
          plot.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank())+
         theme(axis.line = element_line(color = 'black'))+xlab("Latitude")+ylab("Elevation in Meters")
     dev.off()
    
     png(file.path(OutputGraphics,"ErrorByElevandLat.png"), width=8, height=6, res=400, units="in")
     ggplot(ErrorByElev,aes(x=Lat,y=Elev,color=AnnualError))+geom_point(size=2.5,
                                                                             alpha=.2)+
       scale_color_gradient2(name="abs(Annual \nError)")+
        theme(axis.text=element_text(size=12),
                         axis.title=element_text(size=15,face="bold"),
                         plot.title=element_text(size=15,face="bold"),
                         legend.title=element_text(size=10),
                         legend.text=element_text(size=10),
                         plot.background = element_blank()
                         ,panel.grid.major = element_blank()
                         ,panel.grid.minor = element_blank()
                         ,panel.border = element_blank())+
       theme(axis.line = element_line(color = 'black'))+xlab("Latitude")+ylab("Elevation in Meters")
     dev.off()
     
    png(file.path(OutputGraphics,"ErrorByDistanceToSnotelandLat.png"), width=8, height=6, res=400, units="in")
    ggplot(ErrorByElev,aes(x=DistToSnotel,y=abs(AnnualError),colour=Lat))+geom_point(size=1.5,
    alpha=.1)+geom_smooth(size=1.2)+ coord_cartesian(ylim=c(.0,.3))+xlab("Distance to SNOTEL")+ylab("abs(Annual Error)")+
         theme_bw()+scale_color_gradientn(colors=rev(inferno(20)))+
         theme(axis.text=element_text(size=12),
                axis.title=element_text(size=15,face="bold"),
                plot.title=element_text(size=15,face="bold"),
                legend.title=element_text(size=10),
          plot.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank())+
         theme(axis.line = element_line(color = 'black'))
     dev.off()
     
    
     
png(file.path(OutputGraphics,"ErrorByDistanceToSnotelandLatAbs.png"), width=8, height=6, res=400, units="in")
    ggplot(ErrorByElev,aes(y=DistToSnotel,x=Lat,colour=abs(AnnualError)))+geom_point(size=2.15,
    alpha=.3)+ ylab("Distance to SNOTEL")+xlab("Latitude")+
         theme_bw()+scale_color_gradientn(colors=rev(inferno(20)),name="abs(Annual \nError)")+
         theme(axis.text=element_text(size=12),
                axis.title=element_text(size=15,face="bold"),
                plot.title=element_text(size=15,face="bold"),
                legend.title=element_text(size=10),
          plot.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank())+
         theme(axis.line = element_line(color = 'black'))
     dev.off()
     
     png(file.path(OutputGraphics,"ErrorByDistanceToSnotelandLat.png"), width=8, height=6, res=400, units="in")     
     ggplot(ErrorByElev,aes(y=DistToSnotel,x=Lat,colour=AnnualError))+
       geom_point(size=2.15, alpha=.9)+ ylab("Distance to SNOTEL")+xlab("Latitude")+
       scale_color_gradient2(name="log(Annual \nError)")+
       theme(axis.text=element_text(size=12),
             axis.title=element_text(size=15,face="bold"),
             plot.title=element_text(size=15,face="bold"),
             legend.title=element_text(size=10),
             plot.background = element_blank()
             ,panel.grid.major = element_blank()
             ,panel.grid.minor = element_blank()
             ,panel.border = element_blank())+
       theme(axis.line = element_line(color = 'black'))
     dev.off()
     
    ggplot(ErrorByElev,aes(x=Elev,y=abs(AnnualError),color=Lat))+geom_point(size=2.3,
    alpha=.1)+geom_smooth(size=1.2)+facet_wrap(~Snotel)+
       scale_color_gradientn(colors=(inferno(20)))+
         theme_bw()+coord_cartesian(ylim=c(0,.4))+
         theme(
          plot.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank())+xlab("Elevation")+ylab("exp(Absolute Annual Error)")+
         theme(axis.line = element_line(color = 'black'))
         
         
    aggregate(abs(ErrorByElev$AnnualError),FUN=median,by=list(Snotel=ErrorByElev$Snotel))
    aggregate(abs(ErrorByElev$AnnualError),FUN=range,by=list(Snotel=ErrorByElev$Snotel))
    
    ggplot(ErrorByElev, aes(AnnualError)) + geom_histogram()+ facet_wrap(~Snotel)+theme_bw()
    ggplot(ErrorByElev, aes(Lat)) + geom_histogram()+ facet_wrap(~Snotel)+theme_bw()
    ggplot(ErrorByElev, aes(Elev)) + geom_histogram()+ facet_wrap(~Snotel)+theme_bw()
    
    AllVic<-data.frame(v412=vic412SWE,v407=vic407SWE)
    AllVic<-na.omit(AllVic)
    plot(AllVic$v407,AllVic$v412,col=changeAlpha("blue",.5),
         pch=16,cex=.5,xlim=c(0,2500),ylim=c(0,2500),
         bty="l",xlab="VIC407",ylab="VIC412")
    lines(lowess(AllVic$v412~AllVic$v407,f=1/500),col="green",lwd=2)
    ClippedSums<-annSums[annSums$vic407<6000,]
    ClippedSums<-na.omit(ClippedSums)
    
    points(ClippedSums$vic407,ClippedSums$vic412,col=changeAlpha("red",.1),
         pch=16,cex=.5)
    abline(0,1) 
    Fit<-lm(ClippedSums$vic407~ClippedSums$vic412)
    lines(lowess(ClippedSums$vic412~ClippedSums$vic407),col="red")
   
    #looking at a random forest 
    RFFit <- randomForest(AnnualError ~ Elev+Lat+Lon+DistToSnotel, data=ErrorByElev, mtry=3,
                          importance=TRUE,nodesize=250)
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
      plot(x=test[,pIdx],y=resp,type="l",ylab="response",ylim=c(-1,1),xlab=names(dat)[pIdx],bty="l")
      abline(h=0)
    }
    par(mfrow=c(2,3))
    for(i in 1:3){
      for(j in (i+1):4){
    n <- 100
    test <- do.call("rbind", replicate(n^2, means, simplify=FALSE))
    yCol <- i
    xCol <-j
    
    test[, yCol] <- rep(seq(mins[yCol], maxs[yCol], length.out=n),each=n)
    test[, xCol] <- rep(seq(mins[xCol], maxs[xCol], length.out=n),times=n)
    test <- as.data.frame(test)
    colnames(test) <- names(means)
    Response<-predict(RFFit,test,type="response")
    
    z <- matrix(Response,ncol=n)
    nrz <- nrow(z)
    ncz <- ncol(z)
    zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
   theta=45
   phi=45
     Col<-inferno(35)
    nbcol <- length(Col)
    # Recode facet z-values into color indices
    facetcol <- cut(zfacet, nbcol)
    
    persp(x=seq(mins[xCol], maxs[xCol], length.out=n),y=seq(mins[yCol], maxs[yCol], length.out=n),
          z=z,theta=theta,phi=phi,col=Col[facetcol],shade=.4,xlab=names(mins)[xCol]
          ,ylab=names(mins)[yCol],zlab="Prediction",
          border=NA,cex.lab=1.3)
    
    }}
    
    #we do have to calculate the correct order for the data before adding the line
    o <- order(ClippedSums$vic407)
    lines(ClippedSums$vic407[o], predict(loess_fit,ClippedSums$vic412[o]), col = "red")
    
    ggplot(ErrorByElev,aes(x=Elev,y=exp(AnnualError),color=Lat))+geom_point(size=1.2,alpha=.05)+geom_smooth()+
       scale_color_gradientn(colors=rev(viridis(20)))+
       theme_bw()+coord_cartesian(ylim=c(.25,1.77))+   #coord_cartesian(ylim(as.vector(c(0,quantile(exp(ErrorByElev$AbsAnnError),.97)))))
         theme(
          plot.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank())+
         theme(axis.line = element_line(color = 'black'))
         
    ggplot(ErrorByElev,aes(x=Lat,y=exp(AnnualError),color=Elev))+geom_point(size=1,alpha=.2)+geom_smooth()+
         scale_color_gradientn(colors=rev(viridis(20)))+coord_cartesian(ylim=c(.25,1.77))+theme_bw()+
         theme(
          plot.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank())+
         theme(axis.line = element_line(color = 'black'))


         
    ggplot(ErrorByElev,aes(x=Lat,y=exp(AnnualError),color=Snotel))+geom_point(size=.3,alpha=.2)+geom_smooth(size=1.2)+
       scale_color_manual(values=c("lightsteelblue","blue4"))+coord_cartesian(ylim=c(.25,1.77))+theme_bw()+
         theme(
          plot.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank())+
         theme(axis.line = element_line(color = 'black'))


       
    ggplot(ErrorByElev,aes(x=Elev,y=Lat,color=AbsAnnError))+geom_point(size=1,alpha=.5)+scale_color_gradientn(colors=rev(viridis(20)))+ facet_grid(~SnotelLoc)
    ggplot(ErrorByElev,aes(x=DistToSnotel,y=Error,color=Elev))+geom_point(size=1,alpha=.1)+geom_smooth(size=1.2)+
        scale_color_gradientn(colors=rev(viridis(20)))
    ggplot(ErrorByElev,aes(x=Elev,y=DistToSnotel,color=Error))+geom_point(size=1,alpha=.1)+scale_color_gradientn(colors=rev(viridis(20)))


d<-data.frame(SNOTEL=c(annSums$SNOTEL,annSums$SNOTEL),
              VIC=c(annSums$vic407,annSums$vic412),
              Model=factor(c(rep("VIC 4.0.7",times=nrow(annSums)),
                              rep("VIC 4.1.2",times=nrow(annSums)))))

ggplot(d,aes(x=SNOTEL,y=VIC,color=Model))+geom_smooth()+geom_abline(intercept=0,slope=1)+
          coord_cartesian(ylim=(c(2000,13000)))+coord_cartesian(xlim=(c(2000,13000)))