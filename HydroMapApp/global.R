options(RCHART_LIB = 'polycharts')
options(RCHART_WIDTH = 800,RCHART_HEIGHT = 300)

load("data/ShinyDatStacksPrettyNames.RData")
MonthlyByStation<-MonthlyByStation[!MonthlyByStation$SiteName%in%
                                     names(table(MonthlyByStation$SiteName))[as.vector(table(MonthlyByStation$SiteName))!=12],]
latitude<-as.numeric(MonthlyByStation$Lat[MonthlyByStation$Month==1])
longitude<-as.numeric(MonthlyByStation$Lon[MonthlyByStation$Month==1])
ids<-MonthlyByStation$SiteName[MonthlyByStation$Month==1]