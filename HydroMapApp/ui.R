
ui = fluidPage(fluidRow(
    column(2,
           selectInput("mapVar", "Variable",
                       choices=c("Snow Water Equivilant","Runoff","Soil Moisture Content","ET"),
                       selected="Snow Water Equivilant")),
    column(2,
           selectInput("mapRCP", "Emissions Path",
                       choices=c("High (RCP 8.5)","Mid Low (RCP 4.5)"),
                       selected="Mid Low (RCP 4.5)")),
    column(2,
           selectInput("mapTime", "Time Period",
                       choices=c("Annual","Jan","Feb","March","April","May","June","July",
                                 "August","September","October","November","December"),
                       selected="1990s")),
    column(2,sliderInput("mapTrans","Transparency", 0, 1,.8)),    
    column(2,
           checkboxInput("diffFromHist", label = "Show difference from historic period", value = FALSE))
  ),
  leafletOutput('Map')
)
