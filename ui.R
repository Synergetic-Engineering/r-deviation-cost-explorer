ui <- shinyUI(fluidPage(
  
  titlePanel("Cost Difference"),
  
  fluidRow(
    wellPanel(
      selectInput("plot_type", "Plot Type: ",
                  c("Bar Chart" = 'plot_bars_cmp',
                    "Line Chart" = 'plot_deviated_baseline'))
    )
  ),
  
  fluidRow(
    column(6,
      selectInput("variable1", "Component 1: ", u$get_variable_names(r$data)),
      textOutput("status1"),
      plotOutput("plot1", brush = brushOpts("plot_brush1", direction = "x"))
    ),
    column(6,
      selectInput("variable2", "Component 2: ", u$get_variable_names(r$data)),
      textOutput("status2"),
      plotOutput("plot2", brush = brushOpts("plot_brush2", direction = "x"))
    )
  ),
  
  fluidRow(
    wellPanel(
      textOutput("comparison")
    )
  )
  
)
)
