
ui = navbarPage("VIC Model Comparison",
  tabPanel("Explore Data",
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
        checkboxInput("showSS", label = "Show SNOTEL and Satellite data", 
                      value = FALSE),
      width=2
    ),
    mainPanel(
      helpText("Monthly VIC model ratios will be plotted in the lower panel",
               "if you click on any point on the map.",
               "When viewing SWE you can optionally add curves from other data sources",
               "by checking the box."),
      leafletOutput('Map'),
      showOutput("myChart", "nvd3"),
      width=10
    )
   
  )
),
tabPanel("Background Information",
         mainPanel(
           includeHTML("include.html"),
           img(src="PairsPlotLinesForStates.png",height=600,width=600),
           img(src="ErrorByElevandLat.png",height=400,width=600),
           img(src="ErrorByDistanceToSnotelandLat.png",height=400,width=600)
           
         )
         )
  )
