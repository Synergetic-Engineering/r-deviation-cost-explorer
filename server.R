server <- shinyServer(function(input, output, session) {
 observeEvent(input$display, {
   output$plot <- renderPlot({
     r$data %>% u$extract(input$variable) %>% u$process_dev() %>% p$plot_bars_cmp()
   })
 })
  
})
