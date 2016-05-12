
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  dat<-NA
  #===== Available Shapefiles Update =======#
  
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
    MyMap<-leaflet() %>% addTiles()  
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
                    opacity = input$mapTrans)%>%
     #addCircleMarkers(lat = latitude, lng = longitude, radius = .3, 
     #                 color="black",layerId=ids) %>% 
     addLegend(pal = palblue,values=c(exp(0),exp(1.2)),
               title="VIC 4.0.7/VIC 4.1.2") %>%
     addLegend(pal = palred, values = c(exp(0),exp(1.2)),
               title="VIC 4.1.2/VIC 4.0.7")
 })
  
})   
