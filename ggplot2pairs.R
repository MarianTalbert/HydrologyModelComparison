ggpairs<-function(dat,alph,pointSize,DevScore,showResp){

  d<-data.frame(x=c(0,1),y=c(0,1))
  offset<-ifelse(showResp,1,0)

  grid.newpage()
  pushViewport(viewport(layout=grid.layout((ncol(dat)-1),(ncol(dat))-1)))
  vplayout<- function(x,y)
    viewport(layout.pos.row=x, layout.pos.col=y)
  if(showResp){
      dat[,ncol(dat)]<-as.numeric(as.character(dat[,ncol(dat)])) 
      for(j in 1:(ncol(dat)-1)){
        print(ggplot(dat, aes_q(x = as.name(names(dat)[j]), 
                          y =as.name(names(dat)[ncol(dat)]),
                          colour=as.name(names(dat)[ncol(dat)]))) + 
          geom_point(alpha=.2) +
          stat_smooth(method="glm", method.args=list(family="binomial"), formula = y ~ ns(x, 2))+
          theme(legend.position="none")+ #scale_color_gradient(low="blue",high="red")+
          theme(panel.grid.minor=element_blank(),
                panel.grid.major=element_blank(),plot.margin=unit(c(3,4,4,4),"mm"),
                axis.text=element_text(size=rel(1.3)),
                axis.title=element_text(size=rel(1.5)))+
          xlab(names(dat)[j])+ggtitle(paste(ifelse(DevScore$GamRan[j],"GAM","GLM"), "% Dev Expl",DevScore$devExp[j]))+
            ylab("Response")+scale_y_continuous(breaks=NULL),
          vp=vplayout(j,1))
      }
  }

  dat[,ncol(dat)]<-as.factor(dat[,ncol(dat)]) 
  for(i in 1:(ncol(dat)-1)){
  #histogram on the diagonal

  g<-ggplot(dat,aes_q(x=as.name(names(dat)[i])))+
  geom_density(fill="indianred",alpha=.7)+
              theme(legend.position = 'none',plot.margin=unit(c(0,0,1,0),"mm"),axis.title=element_text(size=rel(2.5)))+
              ylab("")+scale_y_continuous(breaks=NULL)+ scale_x_continuous(breaks=NULL)
  print(g,vp=vplayout(i,i+offset))
  #pairs plot below the diagonal
    if(i>1){
    for(j in 1:(i-1)){

         g<-ggplot(dat, aes_q(x = as.name(names(dat)[j]),
                 y = as.name(names(dat)[i]),
                 colour=as.name(names(dat)[ncol(dat)])))+
                 geom_point(size=pointSize, alpha = alph)+geom_smooth(method="lm",se=FALSE)+ theme(legend.position = 'none')+
               theme(panel.grid.minor=element_blank(),
                 panel.grid.major=element_blank(),
                 axis.title=element_text(size=rel(2.5)),
                 plot.margin=unit(c(0,0,1,1),"mm"),
                 axis.text.y = element_text(angle = 90, hjust = 1))+
               scale_y_discrete(breaks=NULL)+scale_x_discrete(breaks=NULL)
         if(j!=1) g<-g+ylab("")
         if(i!=(ncol(dat)-1)) g<-g+xlab("")

        print(g,vp=vplayout(i,j+offset))
    }  
  }
  #color above the diagonal
    Cols<-c("white",colorRampPalette(colors=c("white","indianred"))(8))
    
    if(i<(ncol(dat)-1)){
      for(j in (i+1):(ncol(dat)-1)){
        Cor<-cor(dat[,i],dat[,j])
        ColIndx<-cut(abs(Cor),breaks=c(0,seq(from=.6,to=1,length=length(Cols))))
        print(
          ggplot(d,  aes(x = x, y = y))+ geom_blank()+
                       theme(panel.background = element_rect(fill = Cols[ColIndx]))+
                       theme(panel.grid.major = element_line(colour = Cols[ColIndx]))+
                       theme(panel.grid.minor = element_line(colour = Cols[ColIndx]),
                       plot.margin=unit(c(0,0,1,0),"mm"))+
                       ylab("")+xlab("")+scale_x_continuous(breaks=NULL)+
                       scale_y_continuous(breaks=NULL)+
                       annotate("text", label= round(Cor,digits=2), x=.5, y=.5,
                                size=15*abs(Cor)),
            vp=vplayout(i,j+offset))
      }  
    }
  }
}

#========================================
#========================================
