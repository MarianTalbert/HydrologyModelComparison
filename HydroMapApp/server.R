
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
    v407ind<-ifelse(input$mapVar=="swe",4,2)
    v412ind<-ifelse(input$mapVar=="swe",5,3)
   
      XYdat<-as.data.frame(cbind(X=input$Map_click$lng,Y=input$Map_click$lat))
      VIC407<-as.vector(extract(rast[[v407ind]],XYdat))[1:12]
      VIC412<-as.vector(extract(rast[[v412ind]],XYdat))[1:12]
      Response=c(VIC407,VIC412)
      Model<-rep(c("VIC407","VIC412"),each=12)
      
      if(input$mapVar=="swe"){
        UWhydro<-as.vector(extract(rast[[6]],XYdat))[1:12] 
        Response<-c(Response,UWhydro)
        Model=c(Model,rep("UWhydro",times=12))
        }
      Dat2Use<-data.frame(Response=as.vector(Response),
                          Month=rep(1:12,times=ifelse(input$mapVar=="swe",3,2)),
                          Model=Model)
      
      
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
        Dat2Use<-data.frame(Response=as.vector(c(Dat$VIC407,Dat$VIC412,Dat$Sat,Dat$SNOTEL,Dat$UW)),
                            Month=as.numeric(rep(Dat$Month,times=ifelse(is.null(id),0,5))),
                            Model=as.character(rep(c("VIC407","VIC412","Satellite","SNOTEL","Univ Wash"),
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
    MyMap<-leaflet() %>% addTiles()%>%setView(-100,42,zoom=4) %>%
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
     addRasterImage(dataset[[RcpChoice]][[TimePeriod]],
                    colors = pal, 
                    opacity = .8)
 })

 observe({
   proxy<-leafletProxy("Map")
    id<-input$Map_marker_click$id
     station<-MonthlyByStation$SiteName
    
     Lon<-as.numeric((MonthlyByStation$Lon[MonthlyByStation$SiteName==id])[1])
     Lat<-as.numeric((MonthlyByStation$Lat[MonthlyByStation$SiteName==id])[1])
     ind<-which((latitude==Lat & longitude==Lon),arr.ind=TRUE) 
     col<-"black"
     rad<-1
     Alph<-.3
     
     if(input$mapVar!="swe") Alph = 0
     browser()
     proxy%>%addCircleMarkers(lat = XYs$Lat, lng = XYs$Lon, radius = rad, 
                              color=col,layerId=ids,opacity=Alph,fillOpacity = Alph)
   
 })

 observe({
   if(XYs$clickedMarkerOrMap=="Map"){
     proxy<-leafletProxy("Map")
     proxy%>%addCircleMarkers(lat=input$Map_click$lat,lng=input$Map_click$lng,col="red",
                        radius=3,opacity=1,layerId="2")
   }
   if(XYs$clickedMarkerOrMap=="Marker"){
     if(!is.null(input$Map_marker_click)){
     proxy<-leafletProxy("Map")
     proxy%>%addCircleMarkers(lat=input$Map_marker_click$lat,
                              lng=input$Map_marker_click$lng,col="red",
                              radius=3,opacity=1,layerId="2")
     }
   }
 })
 
})   
