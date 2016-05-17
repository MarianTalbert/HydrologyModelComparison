
ui = fluidPage(
  h4("Comparing Hydrology Model Output"),
  sidebarLayout(
    sidebarPanel(
      
         selectInput("mapVar", "Variable",
                     choices=c("Snow Water Equivilant"="swe","Runoff"="runoff",
                               "Soil Moisture Content"="smc"),
                     selected="swe"),
    
         selectInput("mapTime", "Time Period",
                     choices=list(Annual=13,January=1,
                                  February=2,March=3,April=4,May=5,
                                  June=6,July=7,August=8,September=9,
                                  October=10,November=11,December=12),
                     selected=13),
        sliderInput("mapTrans","Transparency", 0, 1,.8),
        checkboxInput("showSS", label = "Show SNOTEL and Satellite data", 
                      value = FALSE),
      width=2
    ),
    mainPanel(
      leafletOutput('Map'),
      showOutput("myChart", "nvd3")
    )
   
  )
)
