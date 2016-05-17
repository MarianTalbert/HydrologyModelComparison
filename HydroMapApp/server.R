
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  output$myChart<-renderChart2({
    #browser()
    Dat<-MonthlyByStation[MonthlyByStation$SiteName==input$station,]
    Dat2Use<-data.frame(Response=as.vector(c(Dat$VIC407,Dat$VIC412,Dat$Sat,Dat$SNOTEL)),
                        Month=as.numeric(rep(Dat$Month,times=4)),
                        Model=as.character(rep(c("VIC407","VIC412","Satellite","SNOTEL"),
                                               each=12)))
            myPlot<-nPlot(Response~Month,data=Dat2Use,group="Model",
                          type = "lineChart")
            myPlot$yAxis(axisLabel="Snow Water Equivilant")
            myPlot$xAxis(axisLabel="Month")
  return(myPlot)                      
  })
  
  #======================================	
  # create the map
    MapLst<-reactive({
        if(input$mapVar=="swe") return(ShinyMapLst[[1]])
        if(input$mapVar=="runoff") return(ShinyMapLst[[2]])
        if(input$mapVar=="smc") return(ShinyMapLst[[3]])
      })
  
  Mapi=reactive({   
    switch(input$mapVar,
           swe = 1,
           runoff = 2,
           smc =3,
           et=4
           )})
  
  MapLab=reactive({
    switch(input$mapVar,
           swe = "Snow Water Equivilant",
           runoff = "Runoff",
           smc ="Soil Moisture Content",
           et ="Evapotranspiration Actual")})
  
 
  output$Map <- renderLeaflet({
    blueCols<-rev(c(colorRampPalette(c("blue","grey96"))(10),
                    "grey96"))
    redCols<-rev(c(colorRampPalette(c("red4","grey96"))(10),"grey96"))
    palblue <- colorBin(blueCols,domain=c(exp(0),exp(1.2)))
    palred <- colorBin(redCols,domain=c(exp(0),exp(1.2)))
    MyMap<-leaflet() %>% addTiles()%>%
      addLegend(pal = palblue,values=c(exp(0),exp(1.2)),
                title="VIC 4.0.7/VIC 4.1.2") %>%
      addLegend(pal = palred, values = c(exp(0),exp(1.2)),
                title="VIC 4.1.2/VIC 4.0.7")  
    return(MyMap)
  })
 observe({
   
   TimePeriod<-as.numeric(input$mapTime)
   dataset <- MapLst()
   RcpChoice<-1
   
   diffMap<-input$diffFromHist
   Title<-MapLab()
   blueCols<-rev(c(colorRampPalette(c("blue","grey96"))(10),
                   "grey96"))
   redCols<-rev(c(colorRampPalette(c("red4","grey96"))(10),"grey96"))
   Colors<-c(rev(blueCols),redCols)
   pal = colorNumeric(Colors,domain=c(-1.2,1.2),
                      na.color = "transparent")
   palblue <- colorBin(blueCols,domain=c(exp(0),exp(1.2)))
   palred <- colorBin(redCols,domain=c(exp(0),exp(1.2)))
   proxy<-leafletProxy("Map")
   proxy %>%
     addRasterImage(dataset[[TimePeriod]][[RcpChoice]],
                    colors = pal, 
                    opacity = input$mapTrans)
 })
 
 observe({
   proxy<-leafletProxy("Map")
  
     station<-MonthlyByStation$SiteName
     Lon<-as.numeric((MonthlyByStation[MonthlyByStation$SiteName==input$station,7])[1])
     Lat<-as.numeric((MonthlyByStation[MonthlyByStation$SiteName==input$station,8])[1])
     ind<-which((latitude==Lat & longitude==Lon),arr.ind=TRUE) 
     col<-rep("black",times=length(longitude))
     rad<-rep(.1,times=length(longitude))
     Alph<-rep(.3,times=length(longitude))
     col[ind]<-"red"
     rad[ind]<-5
     Alph[ind]<-1
     if(input$mapVar!="swe")  Alph<-rep(0,times=length(longitude))
     
     proxy%>%addCircleMarkers(lat = latitude, lng = longitude, radius = rad, 
                              color=col,layerId=ids,opacity=Alph,fillOpacity = Alph)
   
 })
 
})   
