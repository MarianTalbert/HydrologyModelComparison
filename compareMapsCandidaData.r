library(fields)
library(ncdf4)
library(raster)
library(rgdal)
library(RColorBrewer)
library(viridis)
library(wesanderson)
library(ggplot2)

source("C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\HydroCode\\myImagePlot.r")
source("C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\HydroCode\\my.filled.contour.r")
source("C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\HydroCode\\convertToRaster.r")
OutputGraphics<-"C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\graphics"
latName <- c("latitude",rep("lat",times=8))
lonName <- c("longitude",rep("lon",times=8))
years <- 1988:1999
ImgLst <- list() 
MonthsToUse<-c(1,2,3,11,12)
AdditionTerm<-10 #to add to the numerator and denominator
BasePaths=c(cmip5Obs = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\cmip5\\hydro\\historical_mon_VIC\\conus_c5.para_v0.monthly",
cmip3arkred = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\arkred\\obs.maurer_2002.arkred.monthly",
cmip3cali = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\cali\\obs.maurer_2002.cali.monthly",
cmip3colo = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\colo\\obs.maurer_2002.colo.monthly", cmip3gbas = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\gbas\\obs.maurer_2002.gbas.monthly",
cmip3gulf = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\gulf\\obs.maurer_2002.gulf.monthly",
cmip3mo = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\mo\\obs.maurer_2002.mo.monthly",
cmip3pnw = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\pnw\\obs.maurer_2002.pnw.monthly",
cmip3riog = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\riog\\obs.maurer_2002.riog.monthly")
 Vars<-"swe"

 #===============================================
 #           getting the cmip3 data cmip5 is also in here but much shorter
 #===============================================
 
Vars<-c("swe","smc","runoff")
plotLabels<-c("SWE","SMC","Runoff")
ShinyMapLst<-list()
for(v in 1:length(Vars)){

    CandidaData<-"C:\\Users\\mtalbert\\Downloads\\SWE_monthly_CONUS_12km_1987-2003.nc"
    SnowSites<-read.csv("C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\SnotelLocs.csv")
     Paths<-paste(BasePaths,".",Vars[v],".",sep="")
     for(p in 1:length(Paths)){
       VarNames<-Vars[v]
       if(Vars[v]=="runoff" & p==1){
         Paths[p]<-gsub("runoff","total_runoff",Paths[p])
         VarNames<-"total_runoff"
       }
       MonthlyImages <- list()
       for(m in 1:12){
           for(yr in years){
               filePath <- paste(Paths[p],yr,".nc",sep="")
               Clim <- nc_open(filePath)

                   if(yr==years[1]){ #do this for the first year only because it takes a bit to caluclate
                     Lat <- ncvar_get(Clim,latName[p])
                     Lon <- ncvar_get(Clim,lonName[p])
                     OutputImg <- matrix(data=0,nrow=length(Lon),ncol=length(Lat))
                  }

                   RastArray <- ncvar_get(Clim, varid=VarNames, start=c(1,1,m),
                               count=c(length(Lon),length(Lat),1))

                    OutputImg <- OutputImg+RastArray
                   nc_close(Clim)
            }
            MonthlyImages[[m]] <- OutputImg/length(years)
        }
        ImgLst[[p]] <- MonthlyImages
     }
    #The above could be fairly easilly generalized but now I have to get the images to match
    #by figuring out how the coordinates of the smaller fit into the larger
       p=1
       filePath <- paste(Paths[p],yr,".nc",sep="")
       Clim <- nc_open(filePath)
       countryLat<- ncvar_get(Clim,latName[p])
       countryLon <- ncvar_get(Clim,lonName[p])
       nc_close(Clim)
       
    #=================================================
    # for VIC 4.0.7 the data is broken down by watershed and
    # must be merged back together
    
     for(p in 2:length(Paths)){
       filePath <- paste(Paths[p],yr,".nc",sep="")
       Clim <- nc_open(filePath)
       regionLat<- ncvar_get(Clim,latName[p])
       regionLon <- ncvar_get(Clim,lonName[p])
       nc_close(Clim)

       OutputImg <- matrix(data=0,nrow=length(countryLon),ncol=length(countryLat))
       LonRng<-which(regionLon[1]==countryLon,arr.ind=TRUE):which(regionLon[length(regionLon)]==countryLon,arr.ind=TRUE)
       LatRng<-which(regionLat[1]==countryLat,arr.ind=TRUE):which(regionLat[length(regionLat)]==countryLat,arr.ind=TRUE)
       for(m in 1:12){
         OutputImg <- matrix(data=NA,nrow=length(countryLon),ncol=length(countryLat))
         OutputImg[LonRng,LatRng]<-ImgLst[[p]][[m]]
         ImgLst[[p]][[m]]<-OutputImg
       }
    }
    #I'm cobining the eight small regions into one region by summing an array with na remove and then
    #masking out areas that were NA in all three
    vic407<-list()
    for(m in 1:12){ #months
    d<-array(data=NA,c(length(countryLon),length(countryLat),8))
     #put the three regions in my array
     for(rgn in 1:8){ d[,,rgn]<-ImgLst[[(rgn+1)]][[m]]}

    combineMap<-apply(d,c(1,2),mean,na.rm=TRUE)
    f<-function(x){sum(is.na(x))==8}
    NAmask<-apply(d,c(1,2),FUN=f)
    NAmask[NAmask==TRUE]<-NA
    vic407[[m]]<-combineMap+NAmask
    }

    #==================================================
    #         Satellite data - for what that's worth
    #==================================================
    if(Vars[v]=="swe"){  #only for swe
        monthlySatellite<-list()

        Clim<-nc_open(CandidaData)
         for(m in 1:12){
                 for(yr in 1:length(years)){
                 if(yr==1){ #do this for the first year only because it takes a bit to caluclate
                           satLat <- ncvar_get(Clim,"lat")
                           satLon <- ncvar_get(Clim,"lon")
                           Time<-ncvar_get(Clim,"time")
                           OutputImg <- array(data=0,c(length(years),length(satLon),ncol=length(satLat)))
                        }
                 ind<-as.vector(which(as.character(Time)==paste(years[yr],ifelse(m<10,"0",""),m,sep=""),arr.ind=TRUE))

                   RastArray<-ncvar_get(Clim,varid="swe",start=c(1,1,ind),
                                     count=c(length(satLon),length(satLat),1))
                    OutputImg[yr,,] <- RastArray

                 }
             monthlySatellite[[m]]<-apply(OutputImg,c(2,3),mean,na.rm=TRUE)
             f<-function(x,count){sum(is.na(x))==count}
              NAmask<-apply(OutputImg,c(2,3),FUN=f,count=length(years))
              NAmask[NAmask==TRUE]<-NA
              monthlySatellite[[m]]<-monthlySatellite[[m]]+NAmask
          }
        nc_close(Clim)
        Satellite<-stack(lapply(monthlySatellite,FUN=convertToRaster,Lon=satLon,Lat=satLat))
    }
    #==============================================
    #       vic412 was the CMIP5 model output
    #==============================================
     vic412<-ImgLst[[1]]
    #vic412<-stack(lapply(ImgLst[[1]],FUN=convertToRaster,Lon=countryLon,Lat=countryLat,RastType="GTiff"))
    #vic407<-stack(lapply(vic407,FUN=convertToRaster,Lon=countryLon,Lat=countryLat,RastType="GTiff"))
    #==============================================
    #          now calculating the ratios
    #==============================================
    Colors<-c(colorRampPalette(c("blue","grey96"))(25),"grey96","grey96",rev(colorRampPalette(c("red4","grey96"))(25)))
      breakRng<-c(-1.2,1.2) #a little more than three times on a log scale
        r<-max(abs(breakRng))
        Breaks<-seq(from=-r,to=r,length=length(Colors)+1)

    OutputLst<-list()

    for(m in 1:12){
        #==============================
        #plotting the three ratios both log and nonlog scale non log I take the greater of the exp(log(ratio)) and add a sign
        #to indicate which was greater

      #=========
       # Vic412 vs Vic407
       MonthlyDiff<-log((vic412[[m]]+10)/(vic407[[m]]+10))

        MonthlyDiff[MonthlyDiff<min(breakRng)]<-min(breakRng)
        MonthlyDiff[MonthlyDiff>max(breakRng)]<-max(breakRng)
       myImagePlot(MonthlyDiff,plotLabels[v],"(VIC 4.1.2 + 10)/(VIC 4.0.7 + 10)","CMIP5CMIP3",m,breakRng,Breaks,countryLat,
         countryLon,OutputGraphics,Colors,SnowSites,LogToAbs=TRUE)
       if(m==1) OutputLst$vic412vic407<-stack(convertToRaster(MonthlyDiff,countryLon,countryLat,RastType="GTiff"))
       else OutputLst$vic412vic407<-addLayer(OutputLst$vic412vic407,convertToRaster(MonthlyDiff,countryLon,countryLat,RastType="GTiff"))

       #========================
       #for swe only
       if(Vars[v]=="swe"){
           #=========
           # Vic412 vs Satellite
           MonthlyDiff<-log((vic412[[m]]+10)/(monthlySatellite[[m]][2:(nrow(monthlySatellite[[m]])-1),2:(ncol(monthlySatellite[[m]])-1)]+10))
             MonthlyDiff[MonthlyDiff<min(breakRng)]<-min(breakRng)
             MonthlyDiff[MonthlyDiff>max(breakRng)]<-max(breakRng)
           myImagePlot(MonthlyDiff,plotLabels[v],"(VIC 4.1.2 + 10)/(Satellite+10)","CMIP5Satellite",m,breakRng,
              Breaks,countryLat,countryLon,OutputGraphics,Colors,SnowSites,LogToAbs=TRUE)

            if(m==1) OutputLst$vic412Sat<-stack(convertToRaster(MonthlyDiff,countryLon,countryLat,RastType="GTiff"))
            else OutputLst$vic412Sat<-addLayer(OutputLst$vic412Sat,convertToRaster(MonthlyDiff,countryLon,countryLat,RastType="GTiff"))

           #Satellite data is recorded mid month so maybe we should compare to the mean of this month and next month in the VICs
           #this actually did much worse for some reason so I've removed it...

           #=========
           # Vic407 vs Satellite
           MonthlyDiff<-log((vic407[[m]]+10)/(monthlySatellite[[m]][2:(nrow(monthlySatellite[[m]])-1),2:(ncol(monthlySatellite[[m]])-1)]+10))
               MonthlyDiff[MonthlyDiff<min(breakRng)]<-min(breakRng)
               MonthlyDiff[MonthlyDiff>max(breakRng)]<-max(breakRng)
           if(m==1) OutputLst$vic407Sat<-stack(convertToRaster(MonthlyDiff,countryLon,countryLat,RastType="GTiff"))
            else OutputLst$vic407Sat<-addLayer(OutputLst$vic407Sat,convertToRaster(MonthlyDiff,countryLon,countryLat,RastType="GTiff"))
           myImagePlot(MonthlyDiff,plotLabels[v],"(VIC 4.0.7 + 10)/(Satellite+10)","CMIP3Satellite",m,breakRng,Breaks,countryLat,countryLon,
              OutputGraphics,Colors,SnowSites,LogToAbs=TRUE)
        }
        if(m==1) OutputLst$vic407<-stack(convertToRaster(vic407[[m]],countryLon,countryLat,RastType="GTiff"))
            else OutputLst$vic407<-addLayer(OutputLst$vic407,convertToRaster(vic407[[m]],countryLon,countryLat,RastType="GTiff"))

        if(m==1) OutputLst$vic412<-stack(convertToRaster(vic412[[m]],countryLon,countryLat,RastType="GTiff"))
            else OutputLst$vic412<-addLayer(OutputLst$vic412,convertToRaster(vic412[[m]],countryLon,countryLat,RastType="GTiff"))
    }

    #Annual summaries of all the datasets
    VIC412AnnSumRast<-Reduce("+",vic412)
      OutputLst$vic412<-addLayer(OutputLst$vic412,convertToRaster(Reduce("+",vic412),
            countryLon,countryLat,RastType="GTiff")
      )
    VIC407AnnSumRast<-Reduce("+",vic407)
      OutputLst$vic407<-addLayer(OutputLst$vic407,convertToRaster(Reduce("+",vic407),
             countryLon,countryLat,RastType="GTiff")
      )

     #=========================
     # and now do the annual
    AnnualSumRast<-log((VIC412AnnSumRast+50)/(VIC407AnnSumRast+50))

       AnnualSumRast[AnnualSumRast<min(breakRng)]<-min(breakRng)
       AnnualSumRast[AnnualSumRast>max(breakRng)]<-max(breakRng)
       
     myImagePlot(AnnualSumRast,plotLabels[v],"log((VIC 4.1.2 + 50)/(VIC 4.0.7 + 50))","CMIP5CMIP3Annual",m=0,breakRng,
          Breaks,countryLat,countryLon,OutputGraphics,Colors,SnowSites,LogToAbs=TRUE)
        OutputLst$vic412vic407<-addLayer(OutputLst$vic412vic407,convertToRaster(AnnualSumRast,countryLon,countryLat,RastType="GTiff"))
        
    if(Vars[v]=="swe"){
        SateliteAnnSumRast<-Reduce("+",monthlySatellite)[2:(nrow(monthlySatellite[[m]])-1),2:(ncol(monthlySatellite[[m]])-1)]
         AnnualSumRast<-log((VIC412AnnSumRast+50)/(SateliteAnnSumRast+50))
             AnnualSumRast[AnnualSumRast<min(breakRng)]<-min(breakRng)
             AnnualSumRast[AnnualSumRast>max(breakRng)]<-max(breakRng)
         myImagePlot(AnnualSumRast,plotLabels[v],"log((VIC 4.1.2 + 50)/(Satellite + 50))","CMIP5CSatelliteAnnual",m=0,breakRng,
              Breaks,countryLat,countryLon,OutputGraphics,Colors,SnowSites,LogToAbs=TRUE)
        OutputLst$vic412Sat<-addLayer(OutputLst$vic412Sat,convertToRaster(AnnualSumRast,countryLon,countryLat,RastType="GTiff"))
        AnnualSumRast<-log((VIC407AnnSumRast+50)/(SateliteAnnSumRast+50))
            AnnualSumRast[AnnualSumRast<min(breakRng)]<-min(breakRng)
            AnnualSumRast[AnnualSumRast>max(breakRng)]<-max(breakRng)
            
         myImagePlot(AnnualSumRast,plotLabels[v],"log((VIC 4.1.2 + 50)/(Satellite + 50))","CMIP3SatelliteAnnual",m=0,breakRng,
              Breaks,countryLat,countryLon,OutputGraphics,Colors,SnowSites,LogToAbs=TRUE)
        OutputLst$vic407Sat<-addLayer(OutputLst$vic407Sat,convertToRaster(AnnualSumRast,countryLon,countryLat,RastType="GTiff"))
    }
    ShinyMapLst[[v]]<-OutputLst
}




