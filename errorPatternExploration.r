 #some plots of the errors 
 #======================================
    # looking at what's causing the error with interactions
    # exp(AnnualError)==1 is a match between the two
    #added 25 to annual 
    
 load("C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\graphics/ggplotExplore")
 library(ggplot2)
     ggplot(ErrorByElev,aes(x=DistToSnotel,y=exp(AnnualError),color=Snotel))+geom_point(size=.1,
    alpha=.1)+geom_smooth(size=1.2)+
       scale_color_manual(values=c("lightsteelblue","blue4"))+
         theme_bw()+coord_cartesian(ylim=c(.25,1.77))+
         theme(axis.text=element_text(size=18),
                axis.title=element_text(size=20,face="bold"),
                plot.title=element_text(size=20,face="bold"),
                legend.title=element_text(size=24),
                legend.text=element_text(size=18))
          plot.background = element_blank()
         ,panel.grid.major = element_blank()
         ,panel.grid.minor = element_blank()
         ,panel.border = element_blank())+xlab("Elevation")+ylab("exp(Absolute Annual Error)")+
         theme(axis.line = element_line(color = 'black'))

    png(file.path(OutputGraphics,"ErrorByElevandLat.png"), width=8, height=6, res=400, units="in")
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
     
     
png(file.path(OutputGraphics,"ErrorByDistanceToSnotelandLat2.png"), width=8, height=6, res=400, units="in")
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
    
    ggplot(ErrorByElev, aes(AnnualError)) + geom_histogram()+ facet_wrap(~Snotel)+theme_bw()
    ggplot(ErrorByElev, aes(Lat)) + geom_histogram()+ facet_wrap(~Snotel)+theme_bw()
    ggplot(ErrorByElev, aes(Elev)) + geom_histogram()+ facet_wrap(~Snotel)+theme_bw()
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

RFFit <- randomForest(exp(AnnualError) ~ Elev+Lat+Lon+DistToSnotel, data=ErrorByElev, mtry=3,
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
      plot(x=test[,pIdx],y=resp,type="l",ylab="response",xlab=names(dat)[pIdx],bty="l",ylim=c(.5,1.5))
      abline(h=1)
       }
       
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