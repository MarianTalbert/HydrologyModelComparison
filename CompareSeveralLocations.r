 library(ncdf4)
 library(ggplot2)
 library(mapproj)
 library(grid)
 OutputGraphics<-"C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\graphics"
#look at several locations unfortunately paths change for differnt watersheds
Paths=c(cmip5Obs = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\cmip5\\hydro\\historical_mon_VIC\\conus_c5.para_v0.monthly.swe.",
         cmip3Obs = "E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed\\mo\\obs.maurer_2002.mo.monthly.swe.")
         
    png(file.path(OutputGraphics,"HazenVICModels.png"),width=900,
                      height=700,type="cairo")
       HazenND<-matrix(c(-102.0625,-101.0625,47.0625,47.7),2,2,byrow=TRUE)
         CompareVics(HazenND,Paths)
   dev.off()
   
     Knife<-matrix(c(-113.9375,-90.1975,37.0625,49.6875),2,2,byrow=TRUE)
       CompareVics(Knife,Paths)
   GreatFallsMT<-matrix(c(-111.625,-111.0,47.825,47.4),2,2,byrow=TRUE)
     CompareVics(GreatFallsMT,Paths)
     
  CodyWY<-matrix(c(-109.0625,-109.2,44.0625,44.7),2,2,byrow=TRUE)
     CompareVics(CodyWY,Paths)
     
  MissionRidgeSD<-matrix(c(-100.0625,-101.0625,44.0,44.5),2,2,byrow=TRUE)
     CompareVics(MissionRidgeSD,Paths)
     
     #for tomorrow look at maps especially for febuary   