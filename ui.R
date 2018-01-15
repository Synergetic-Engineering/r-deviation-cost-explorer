ui <- shinyUI(fluidPage(
  
  titlePanel("Cost Difference"),
  
  mainPanel(
    
    tabsetPanel(id = "mainTabset",
                tabPanel("Select Project", 
                         # value is always yyyy-mm-dd, even if the display format is different
                         #dateInput("date_1", "Date #1", value = "2017-12-29", format = "dd/mm/yyyy"),
                         dateInput("date_1", "Date #1", value = Sys.time(), format = "dd/mm/yyyy"),
                         timeInput("time_1", "Time #1", value = strptime("12:34:56", "%T")), 
                         dateInput("date_2", "Date #2", value = Sys.time(), format = "dd/mm/yyyy"),
                         timeInput("time_2", "Time #2", value = strptime("12:34:56", "%T")), 
                         #textInput("my_output"),
                         selectInput("variable", "Component: ",
                                     u$get_variable_names(r$data)),
                         actionButton("display", "Submit"),
                         textOutput("Status"),
                         plotOutput("plot")
                )
  )
)
)
)
