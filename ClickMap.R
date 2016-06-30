require(rCharts)
require(shiny)
runApp(list(
  ui =  fluidPage(
    leafletOutput("map")
  ),
  server= function(input, output) {
    data <- reactiveValues(clickedMarker=NULL)
    # produce the basic leaflet map with single marker
    output$map <- renderLeaflet(
      leaflet() %>%
       addProviderTiles("CartoDB.Positron") %>%
        addCircleMarkers(lat = 54.406486, lng = -2.925284)    
    )
    
    # observe the marker click info and print to console when it is changed.
    observeEvent(input$map_marker_click,{
      data$clickedMarker <- input$map_marker_click
      print(data$clickedMarker)}
    )
    observeEvent(input$map_click,{
      browser()
      data$clickedMarker <- NULL
      print(data$clickedMarker)})
  }
))
