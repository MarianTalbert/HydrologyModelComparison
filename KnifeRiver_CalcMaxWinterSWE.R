# Take monthly swe data for a given coordinate and find the max winter
# (December-April) swe value in each year; 
# Then find the mean max winter snowpack within past (1950-1999)
# and future (2020-2049) periods, and calculate the ratio of future mean
# to past mean.  

## Fields to edit before running the script for Cmip3 vs. Cmip5 ##
 setwd("C:\\Users\\mtalbert\\Desktop\\HydrologyProblem")
  cmip3rawdata.filename <- "sweProjCmip3headers.csv"
  cmip5rawdata.filename<- "sweProjCmip5headers.csv"
  output.filename  <-"swe_Cmip3_MaxWinterSnowpack.csv"

  
#### SETUP AND PREPARE DATA ####

# Load packages
  library(plyr)
  library(dplyr)
  library(reshape2)
  library(ggplot2)

# Read in raw data
  cmip3swe <- read.csv(cmip3rawdata.filename)  #file in default directory
   cmip5swe <- read.csv(cmip5rawdata.filename)

#converting factors to numeric
   indx<-sapply(cmip5swe,is.factor)
   cmip5swe[indx] <- lapply(cmip5swe[indx], function(x) as.numeric(as.character(x)))
   cmip3monthly<-aggregate(cmip3swe[,c(3:ncol(cmip3swe))],FUN=mean,list(Month=cmip3swe$month))
   cmip5monthly<-aggregate(cmip5swe[,c(3:ncol(cmip5swe))],FUN=mean,list(Month=cmip5swe$month))

  cmip3Quants<- apply(cmip3monthly,1,quantile,c(.05,.5,.95))
  cmip5Quants<- apply(cmip5monthly,1,quantile,c(.05,.5,.95),na.rm=TRUE)
  
Col<-c("blue","red","green")
       color.box<-col2rgb(Col,alpha=TRUE)
                    color.box[4,]<-10
                    temp.fct<-function(a){return(rgb(red=a[1],green=a[2],blue=a[3],alpha=a[4]))}
  Col<-apply(color.box/255,2,temp.fct)
  
plot(c(1,12),c(0,80),type="n",xlab="Month",ylab="SWE")
for(yr in 1950:2015){
    for(model in 4:ncol(cmip5swe)){
      lines(1:12,cmip5swe[cmip5swe$year==yr,model],col=Col[1])
    }
    for(model in 4:ncol(cmip3swe)){
      lines(1:12,cmip3swe[cmip3swe$year==yr,model],col=Col[2])
    }
}

  lines(1:12,cmip3Quants[2,],lwd=5,col="red")
  lines(1:12,cmip5Quants[2,],lwd=5,col="blue")

legend('topright',fill=c("blue","red"),legend=c("CMIP5 VIC","CMIP3 VIC"))

d<-data.frame(SWE=c(as.vector(as.matrix(cmip3swe[,3:ncol(cmip3swe)])),
                 as.vector(as.matrix(cmip5swe[,3:ncol(cmip5swe)]))),
           VIC=as.factor(
                c(rep("CMIP3",times=(ncol(cmip3swe)-2)*nrow(cmip3swe)),
                  rep("CMIP5",times=(ncol(cmip5swe)-2)*nrow(cmip5swe)))),
           Month=as.factor(
                c(rep(cmip3swe$month,times=(ncol(cmip3swe)-2)),
                  rep(cmip5swe$month,times=(ncol(cmip5swe)-2)))))
           
p<-ggplot(d,aes(x=Month,y=SWE,colour=VIC))+geom_boxplot()



   cmip5yr<-aggregate(cmip5swe,FUN=sum,list(Year=cmip5swe$year))
   cmip3yr<-aggregate(cmip3swe,FUN=sum,list(Year=cmip3swe$year))



   plot(c(1950,2015),c(0,65),type="n")
  for(i in 4:ncol(cmip5yr)){
    lines(cmip5yr[,1],cmip5yr[,i],col=Col[1])
  }
  lines(cmip5yr[,1],apply(cmip5yr[,c(4:ncol(cmip5yr))],1,mean),col="blue",lwd=3)
  lines(cmip3yr[,1],apply(cmip3swe[cmip3swe$month==12,],1,mean),col="red")


  # Change [year] for December to match the year of the other 4 winter months (Jan-Apr)
  cmip3swe$year[cmip3swe$month==12] <- (cmip3swe$year[cmip3swe$month==12] + 1)
  cmip5swe$year[cmip5swe$month==12] <- (cmip5swe$year[cmip5swe$month==12] + 1)
  
  # Get rid of all May-Nov (non-winter) data, which is not needed
  cmip3swe <- filter(cmip3swe, month<=4 | month==12)
  cmip5swe <- filter(cmip5swe, month<=4 | month==12)
  
  cmip3swe$month[cmip3swe$month==12]<-0
  cmip5swe$month[cmip5swe$month==12]<-0

  #averaging over all of the models
  cmip5mean<-apply(cmip5swe[,c(3:ncol(cmip5swe))],1,mean)
  cmip3mean<-apply(cmip3swe[,c(3:ncol(cmip3swe))],1,mean)


 #Plot by year
  plot(c(1950,2015),c(0,60),type="n")

  for(i in 3:ncol(cmip3swe)){
   lines(cmip3swe[,1]+cmip3swe[,2]/5,cmip3swe[,i],col=Col[1])
  }
  lines(cmip3swe[,1]+cmip3swe[,2]/5,cmip3mean,col="blue",lwd=3)

  for(i in 3:ncol(cmip5swe)){
   lines(cmip5swe[,1]+cmip5swe[,2]/5,cmip5swe[,i],col=Col[2])
  }
  lines(cmip5swe[,1]+cmip5swe[,2]/5,cmip5mean,col="red",lwd=3)
#I think this needs a smooth
 lines(dat$Time[dat$Model==names(Paths)[1]],dat$Ts[dat$Model==names(Paths)[1]],lwd=3,col="orange")
 lines(dat$Time[dat$Model==names(Paths)[2]],dat$Ts[dat$Model==names(Paths)[2]],lwd=3,col="green")


# Add a factor to easily group by period
  cmip3swe$period <- NA
    cmip3swe$period[cmip3swe$year>=1950 & cmip3swe$year<=1999] <- "past"
    cmip3swe$period[cmip3swe$year>=2020 & cmip3swe$year<=2049] <- "future"
   cmip5swe$period <- NA
    cmip5swe$period[cmip5swe$year>=1950 & cmip5swe$year<=1999] <- "past"
    cmip5swe$period[cmip5swe$year>=2020 & cmip5swe$year<=2049] <- "future"



# Convert to long form for easier grouping and manipulation
  swe.l <- melt(cmip3swe,
                id.vars=c("period", "year", "month"), 
                variable.name="model", 
                value.name="swe")
  

#### CALCULATE SUMMARY STATISTICS ####
# Find maximum swe value in each year for each mod
  swe.l <- group_by(swe.l, period, year, model)
  max.swe <- summarize(swe.l, max.swe = max(swe, na.rm=TRUE))

# Calculate summary statistics by period (mean of the max.swe value across all years in the period)
  max.swe.sub <- filter(max.swe, !is.na(period))  #for this calculation, don't need years that are not in one of the periods.
  max.swe.sub <- group_by(max.swe.sub, period, model)
  
  period.stats <- summarize(max.swe.sub, 
                           avg.wintermax = mean(as.numeric(max.swe)))

  # Calculate the future:past ratio
    # First have to convert to wide form
      perstats.w <- dcast(period.stats, model~period, value.var="avg.wintermax")
    # Now calculate the quotient
      perstats.w <- mutate(perstats.w, ratio=future/past)
    # (Here is where I would merge in model grouping data, in order to  
    #  calculate summary stats by model type - hot/wet, hot/dry, etc.)
    # Now back to long form (for combining with max.swe)
      perstats.l <- melt(perstats.w, id.vars="model", variable.name="year", value.name="max.swe")
      rm(perstats.w)
    # Rearrange column order to match max.swe
      perstats.l <- perstats.l[, c("year", "model", "max.swe")]

# Combine yearly + summary data in one df
  annual.summ.l <- rbind(max.swe[, 2:4], perstats.l)  #only use columns 2:4 of max.swe because we no longer need [period]
  annual.summ.l <- arrange(ungroup(annual.summ.l), year, model)  #sort by year, then by model.

  # ...And convert back to wide format as requested by JF
  annual.summ.w <- dcast(annual.summ.l, year~model, value.var="max.swe")


#### WRITE OUTPUT #### 
  write.csv(annual.summ.w, output.filename, row.names=FALSE)
