ui <- bootstrapPage(
  tags$style(type="text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("Map", width="100%", height="100%"),
  absolutePanel(top=10, right=10,
                selectInput("location", "Community", c("", locs$loc), selected=""),
                conditionalPanel("input.location !== null && input.location !== ''",
                                 actionButton("button_plot_and_table", "View Plot/Table", class="btn-block"))
  )
)
