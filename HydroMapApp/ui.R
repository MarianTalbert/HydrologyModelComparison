
ui = fluidPage(fluidRow(
    column(2,
           selectInput("mapVar", "Variable",
                       choices=c("swe","runoff","smc","et"),
                       selected="swe")),
    column(2,
           selectInput("mapTime", "Time Period",
                       choices=list(Annual=13,January=1,
                                    February=2,March=3,April=4),
                       selected=13)),
    column(2,sliderInput("mapTrans","Transparency", 0, 1,.8)) 
  ),
  leafletOutput('Map')
)
