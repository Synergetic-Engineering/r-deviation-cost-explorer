# ------------------------------------------------------------------------------------------------------------------
# Filename: ui.R
# Purpose: Analytics example 
# Created: 8 Jan 2018
# ------------------------------------------------------------------------------------------------------------------

server <- shinyServer(function(input, output, session) {
 observeEvent(input$display, {
   output$plot <- renderPlot({
     data %>% extract(input$variable) %>% process_dev() %>% plot_bars_cmp()
   })
 })
  
})