ui <- shinyUI(fluidPage(
  
  titlePanel("Cost Difference"),
  
  fluidRow(
    column(12, checkboxInput("two_components", "Compare two components", TRUE))
  ),
  
  # Dropdown selections
  fluidRow(
    column(6,
      wellPanel(
        uiOutput('unit1'),
        uiOutput('d11'),
        uiOutput('deviated_variable1'),
        uiOutput('measure1')
      )),
    column(6,
      conditionalPanel(condition = "input.two_components == true",
        wellPanel(
           uiOutput('unit2'),
           uiOutput('d12'),
           uiOutput('deviated_variable2'),
           uiOutput('measure2')
        )
      ))
  ),
  
  fluidRow(
    wellPanel(
      selectInput("plot_type", "Plot Type: ",
                  c("Line Chart" = "line_chart",
                    "Bar Chart" = "bar_chart"))
    )
  ),
  
  conditionalPanel(condition = "input.two_components == true",
     fluidRow(
       column(12, tags$h3("Across Components"))
     ),
     fluidRow(
       column(6,
              plotOutput("plot1", brush = brushOpts("plot_brush1", direction = "x")),
              wellPanel(textOutput("status1"))
       ),
       column(6,
              plotOutput("plot2", brush = brushOpts("plot_brush2", direction = "x")),
              wellPanel(textOutput("status2"))
       )
     ),
     
     fluidRow(
       wellPanel(
         plotOutput("reference_plot")
       )
     ),
     
     fluidRow(
       wellPanel(
         textOutput("comparison")
       )
     )
  ),
  
  conditionalPanel(condition = "input.two_components == false",
     fluidRow(
       column(12, tags$h3("Across Time"))
     ),
     fluidRow(
       wellPanel(
         plotOutput("single_comparison_plot")
       )
     ),
     fluidRow(
       wellPanel(
         plotOutput("timeline_plot",
                    brush = brushOpts("timeline_brush", direction = "x"))
       )
     )
  )

))
