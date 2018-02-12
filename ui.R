ui <- shinyUI(fluidPage(
  
  titlePanel("Cost Difference"),
  
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
      wellPanel(
         uiOutput('unit2'),
         uiOutput('d12'),
         uiOutput('deviated_variable2'),
         uiOutput('measure2')
      ))
  ),
  
  fluidRow(
    wellPanel(
      selectInput("plot_type", "Plot Type: ",
                  c("Line Chart" = "line_chart",
                    "Bar Chart" = "bar_chart"))
    )
  ),
  
  tabsetPanel(type = "tabs",
              tabPanel("Across Components",
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
            tabPanel("Across Time",
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
  )

))
