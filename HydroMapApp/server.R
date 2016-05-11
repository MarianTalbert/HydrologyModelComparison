
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
  MapPal1=reactive({
    
      pal = colorNumeric("OrRd",domain=c(19,100),
                         na.color = "transparent")
   
    return(pal)
  })
  MapPal2=reactive({
    
    pal = colorNumeric("Blues",domain=c(19,100),
                       na.color = "transparent")
    
    return(pal)
  })
  output$Map <- renderLeaflet({
    #This is getting to be a mess I should probably write out the rasters
    #to a the file system and construct their names from the input choices
   
    TimePeriod<-as.numeric(input$mapTime)
   
    RcpChoice<-1
  
    diffMap<-input$diffFromHist
    dataset <- MapLst()
    Title<-MapLab()
    #colRange<-VarRng[[Mapi]]
    
    if(!inherits(dataset,"list")){ 
      #for elevation the options are a bit more limted
      dataset<-list(list(dataset))
      TimePeriod=1
      diffMap=FALSE
    }
    
 
        dataset[[TimePeriod]][[RcpChoice]]<-
          (dataset[[TimePeriod]][[RcpChoice]]-dataset[[1]][[1]])
        Title<-"Change in Temperature"
    
    MyMap<-leaflet() %>% addTiles() %>%  addRasterImage(dataset[[TimePeriod]][[RcpChoice]],
                  colors = MapPal1(), 
                  opacity = input$mapTrans) %>%
      addLegend(pal = MapPal1(), values = VarRng[[1]],title=Title) %>%
      addLegend(pal = MapPal2(), values = VarRng[[1]],title=Title)
   
    return(MyMap)
  })

})   
