server <- shinyServer(function(input, output, session) {
  output$plot <- renderPlot({
    r$data %>% u$extract(input$variable) %>% p$plot_bars_cmp()
  })
  
  output$status <- renderText({
    toDate <- function(d) {
      d %>% as.numeric() %>% round(1) %>% as.Date(origin = "1970-01-01")
    }
    start_date <- toDate(input$plot_brush$xmin)
    end_date <- toDate(input$plot_brush$xmax)
    paste0("Start date: ", start_date, " - ",
           "End date: ", end_date, " - ",
           "Average cost: ",
            r$data %>%
            u$extract(input$variable) %>%
            u$filter_by_date(c(start_date, end_date)) %>%
            u$get_average_cost())
  })
  
})
