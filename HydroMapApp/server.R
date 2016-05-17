
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  XYs <- reactiveValues(
    Lat = latitude,
    Lon = longitude,
    clickedMarkerOrMap="Marker"
  )

  observeEvent(input$Map_marker_click,
               {XYs$clickedMarkerOrMap <- "Marker"})
  
  observeEvent(input$Map_click,
               {XYs$clickedMarkerOrMap <- "Map"})
  
  output$myChart<-renderChart2({
  
  if(XYs$clickedMarkerOrMap=="Map"){
    #I think I need the actual VIC 4.0.7 and VIC 4.1.2 data here
    #makes me feel like I should switch to a rasterstack
    rast<-MapLst()
      XYdat<-as.data.frame(cbind(X=input$Map_click$lng,Y=input$Map_click$lat))
      VIC407<-c(extract(rast[[1]][[4]],XYdat),extract(rast[[2]][[4]],XYdat),
                extract(rast[[3]][[4]],XYdat),extract(rast[[4]][[4]],XYdat),
                extract(rast[[5]][[4]],XYdat),extract(rast[[6]][[4]],XYdat),
                extract(rast[[7]][[4]],XYdat),extract(rast[[8]][[4]],XYdat),
                extract(rast[[9]][[4]],XYdat),extract(rast[[10]][[4]],XYdat),
                extract(rast[[11]][[4]],XYdat),extract(rast[[12]][[4]],XYdat))
      VIC412<-c(extract(rast[[1]][[5]],XYdat),extract(rast[[2]][[5]],XYdat),
                extract(rast[[3]][[5]],XYdat),extract(rast[[4]][[5]],XYdat),
                extract(rast[[5]][[5]],XYdat),extract(rast[[6]][[5]],XYdat),
                extract(rast[[7]][[5]],XYdat),extract(rast[[8]][[5]],XYdat),
                extract(rast[[9]][[5]],XYdat),extract(rast[[10]][[5]],XYdat),
                extract(rast[[11]][[5]],XYdat),extract(rast[[12]][[5]],XYdat))
      
      Dat2Use<-data.frame(Response=as.vector(c(VIC407,VIC412)),
                          Month=as.numeric(rep(1:12,times=2)),
                          Model=as.character(rep(c("VIC407","VIC412"),
                                                 each=12)))
      myPlot<-nPlot(Response~Month,data=Dat2Use,group="Model",
                    type = "lineChart")
      myPlot$yAxis(axisLabel="Snow Water Equivilant")
      myPlot$chart(margin = list(left = 100))
      myPlot$xAxis(axisLabel="Month")
      return(myPlot)
  }  else{
    id<-input$Map_marker_click$id
    if(input$mapVar!="swe") id<-NULL
    Dat<-MonthlyByStation[MonthlyByStation$SiteName==id,]
    if(input$showSS){
        Dat2Use<-data.frame(Response=as.vector(c(Dat$VIC407,Dat$VIC412,Dat$Sat,Dat$SNOTEL)),
                            Month=as.numeric(rep(Dat$Month,times=ifelse(is.null(id),0,4))),
                            Model=as.character(rep(c("VIC407","VIC412","Satellite","SNOTEL"),
                                                   each=ifelse(is.null(id),0,12))))
    } else{
        Dat2Use<-data.frame(Response=as.vector(c(Dat$VIC407,Dat$VIC412)),
                            Month=as.numeric(rep(Dat$Month,times=ifelse(is.null(id),0,2))),
                            Model=as.character(rep(c("VIC407","VIC412"),
                                                   each=ifelse(is.null(id),0,12))))
    }
    myPlot<-nPlot(Response~Month,data=Dat2Use,group="Model",
                  type = "lineChart")
    myPlot$yAxis(axisLabel=paste("Snow Water Equivilant",id))
    myPlot$chart(margin = list(left = 100))
    myPlot$xAxis(axisLabel="Month")
    return(myPlot)      
  }                
 
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
      addProviderTiles("CartoDB.Positron") %>%
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
    id<-input$Map_marker_click$id
     station<-MonthlyByStation$SiteName
     Lon<-as.numeric((MonthlyByStation[MonthlyByStation$SiteName==id,7])[1])
     Lat<-as.numeric((MonthlyByStation[MonthlyByStation$SiteName==id,8])[1])
     ind<-which((latitude==Lat & longitude==Lon),arr.ind=TRUE) 
     col<-rep("black",times=length(longitude))
     rad<-rep(.1,times=length(longitude))
     Alph<-rep(.3,times=length(longitude))
     col[ind]<-"red"
     rad[ind]<-5
     Alph[ind]<-1
     if(input$mapVar!="swe")  Alph<-rep(0,times=length(longitude))
     
     proxy%>%addCircleMarkers(lat = XYs$Lat, lng = XYs$Lon, radius = rad, 
                              color=col,layerId=ids,opacity=Alph,fillOpacity = Alph)
   
 })
 
})   
