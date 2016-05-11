
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  dat<-NA
  #===== Available Shapefiles Update =======#
  
  #======================================	
  # create the map
 
  
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
  
  MapLst<-reactive({
    if(input$mapVar=="swe") return(ShinyMapLst[[1]])
    if(input$mapVar=="runoff") return(ShinyMapLst[[2]])
    if(input$mapVar=="smc") return(ShinyMapLst[[3]])
  })
  
  output$Map <- renderLeaflet({
    #This is getting to be a mess I should probably write out the rasters
    #to a the file system and construct their names from the input choices
   
    TimePeriod<-as.numeric(input$mapTime)
   
    RcpChoice<-1
  
    diffMap<-input$diffFromHist
    dataset <- MapLst()
    Title<-MapLab()
    
    
    if(!inherits(dataset,"list")){ 
      #for elevation the options are a bit more limted
      dataset<-list(list(dataset))
      TimePeriod=1
      diffMap=FALSE
    }
    
      blueCols<-rev(c(colorRampPalette(c("blue","grey96"))(10),
                      "grey96"))
      redCols<-rev(c(colorRampPalette(c("red4","grey96"))(10),"grey96"))
      Colors<-c(rev(blueCols),redCols)
      pal = colorNumeric(Colors,domain=c(-1.2,1.2),
                         na.color = "transparent")
      palblue <- colorBin(blueCols,domain=c(exp(0),exp(1.2)))
      palred <- colorBin(redCols,domain=c(exp(0),exp(1.2)))
 
        dataset[[TimePeriod]][[RcpChoice]]<-
          (dataset[[TimePeriod]][[RcpChoice]]-dataset[[1]][[1]])
        Title<-"Change in Temperature"
    
    MyMap<-leaflet() %>% addTiles() %>%  addRasterImage(dataset[[TimePeriod]][[RcpChoice]],
                  colors = pal, 
                  opacity = input$mapTrans) %>%
      addLegend(pal = palblue,values=c(exp(0),exp(1.2)),
                title="VIC 4.0.7/VIC 4.1.2") %>%
      addLegend(pal = palred, values = c(exp(0),exp(1.2)),title="VIC 4.1.2/VIC 4.0.7")
   
    return(MyMap)
  })

})   
