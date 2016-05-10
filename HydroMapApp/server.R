
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  dat<-NA
  #===== Available Shapefiles Update =======#
  
  #======================================	
  # create the map
  values <- reactiveValues(rangeIndx=1)
  Mapi=reactive({   
    switch(input$mapVar,
           Temperature = 1,
           Precipitation = 2,
           Elevation =3)})
  
  MapLab=reactive({
    switch(input$mapVar,
           Temperature = "Average Annual Temperature (F)",
           Precipitation = "Total Annual Precipitation (In/Yr)",
           Elevation ="Elevation")})
  
  MapLst<-reactive({
    if(input$mapVar=="Temperature") return(ShinyMapLst[[1]])
    if(input$mapVar=="Precipitation") return(ShinyMapLst[[2]])
    if(input$mapVar=="Elevation") return(ShinyMapLst[[3]])
  })
  MapPal=reactive({
    
    values$rangeIndx<-switch(input$mapVar,
                             Temperature = 1,
                             Precipitation = 2,
                             Elevation =3)
    
    if(input$mapVar=="Temperature"& !input$diffFromHist)
      pal = colorNumeric("OrRd", VarRng[[values$rangeIndx]],
                         na.color = "transparent")
    if(input$mapVar=="Temperature"& input$diffFromHist){
      values$rangeIndx<-5
      #browser()
      pal = colorNumeric("YlOrRd", VarRng[[values$rangeIndx]],
                         na.color = "transparent")
    }
    if(input$mapVar=="Precipitation" & !input$diffFromHist)
      pal = colorNumeric("BuGn", VarRng[[values$rangeIndx]],
                         na.color = "transparent")
    if(input$mapVar=="Precipitation" & input$diffFromHist){
      values$rangeIndx<-4
      pal = colorNumeric("BrBG", VarRng[[values$rangeIndx]],
                         na.color = "transparent")
    }
    if(input$mapVar=="Elevation") 
      pal = colorNumeric(terrain.colors(10), VarRng[[values$rangeIndx]],
                         na.color = "transparent")
    return(pal)
  })
  output$Map <- renderLeaflet({
    #This is getting to be a mess I should probably write out the rasters
    #to a the file system and construct their names from the input choices
    TimePeriod<-switch(input$mapTime,
                       "1990s"=1,
                       "2040s"=2,
                       "2080s"=3)
    if(input$mapVar=="Elevation") TimePeriod<-1
    RcpChoice<-1
    if(TimePeriod!=1){
      RcpChoice<-switch(input$mapRCP,
                        "High (RCP 8.5)"=2,
                        "Mid Low (RCP 4.5)"=1)}
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
    
    if(TimePeriod!=1 & diffMap){
      if(Title=="Total Annual Precipitation"){
        dataset[[TimePeriod]][[RcpChoice]]<-
          100*(dataset[[TimePeriod]][[RcpChoice]]-dataset[[1]][[1]])/dataset[[1]][[1]]
        Title<-"Percent Change in Precipitation"
        
      }
      else{ 
        
        dataset[[TimePeriod]][[RcpChoice]]<-
          (dataset[[TimePeriod]][[RcpChoice]]-dataset[[1]][[1]])
        Title<-"Change in Temperature"
        
      }
    }
    
    
    MyMap<-leaflet() %>% addTiles() %>%  addRasterImage(dataset[[TimePeriod]][[RcpChoice]], colors = MapPal(), 
                                                        opacity = input$mapTrans) %>%
      addLegend(pal = MapPal(), values = VarRng[[values$rangeIndx]],title=Title)
   
    return(MyMap)
  })

})   
