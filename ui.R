ui <- shinyUI(fluidPage(
  
  titlePanel("Cost Difference"),
  
  fluidRow(
    column(6,
      selectInput("variable1", "Component 1: ", u$get_variable_names(r$data)),
      textOutput("status1"),
      plotOutput("plot1", brush = "plot_brush1")
    ),
    column(6,
      selectInput("variable2", "Component 2: ", u$get_variable_names(r$data)),
      textOutput("status2"),
      plotOutput("plot2", brush = "plot_brush2")
    )
  ),
  
  fluidRow(
    textOutput("comparison")
  )
  
)
)
