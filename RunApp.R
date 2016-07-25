library(shiny)
library(leaflet)
library(raster)
library(rCharts)

options(RCHART_LIB = 'polycharts')
options(RCHART_WIDTH = 800,RCHART_HEIGHT = 300)
setwd("C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\HydroCode")
load("C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\graphics\\ShinyDatNewStacks.RData")
latitude<-as.numeric(MonthlyByStation$Lat[MonthlyByStation$Month==1])
longitude<-as.numeric(MonthlyByStation$Lon[MonthlyByStation$Month==1])
ids<-MonthlyByStation$SiteName[MonthlyByStation$Month==1]


runApp("HydroMapApp")


MyMap<-leaflet() %>% addTiles()%>%setView(-100,42,zoom=4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = palblue,values=c(exp(0),exp(1.2)),
            title="VIC 4.0.7/VIC 4.1.2") %>%
  addLegend(pal = palred, values = c(exp(0),exp(1.2)),
            title="VIC 4.1.2/VIC 4.0.7")%>%
  addRasterImage(ShinyMapLst[[1]][[1]][[1]],
                 colors = pal, 
                 opacity = .8)


runApp("ClickOnPoints")
blueCols<-rev(c(colorRampPalette(c("blue","grey96"))(10),
          "grey96"))
redCols<-rev(c(colorRampPalette(c("red4","grey96"))(10),"grey96"))
Colors<-c(rev(blueCols),redCols)
pal = colorNumeric(Colors,domain=c(-1.2,1.2),
                   na.color = "transparent")
palblue <- colorBin(blueCols,domain=c(exp(0),exp(1.2)))
palred <- colorBin(redCols,domain=c(exp(0),exp(1.2)))
leaflet() %>% addTiles() %>% setView(-71,42,zoom=2)
addRasterImage(ShinyMapLst[[1]][[1]][[1]],
                                                    colors = pal, 
                                                    opacity = .9) %>%
  addLegend(pal = palblue,values=c(exp(0),exp(1.2)),
            title="VIC 4.0.7/VIC 4.1.2") %>%
  addLegend(pal = palred, values = c(exp(0),exp(1.2)),title="VIC 4.1.2/VIC 4.0.7") %>% 
  