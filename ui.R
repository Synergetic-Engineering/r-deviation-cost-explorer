ui <- shinyUI(fluidPage(
  
  titlePanel("Cost Difference"),
  
  mainPanel(
    
    tabsetPanel(id = "mainTabset",
                tabPanel("Select Project", 
                         selectInput("variable", "Component: ",
                                     u$get_variable_names(r$data)),
                         textOutput("status"),
                         plotOutput("plot", brush = "plot_brush")
                )
  )
)
)
)
