server <- shinyServer(function(input, output, session) {
  
  # Get the input/output names of a plot
  #
  # Return input/output names of a given plot as a list with
  # plot, plot_brush, variable, status. Given an id.
  names <- function(id) {
    name_of <- function(el, id) { paste0(el, id) }
    return(list(plot = name_of("plot", id),
                plot_brush = name_of("plot_brush", id),
                variable = name_of("variable", id),
                status = name_of("status", id)
                )
           )
  }
  
  # Get date range as a vector pair: c(<start>, <end>)
  get_dates <- function(id) {
    plot_brush <- names(id)$plot_brush
    toDate <- function(d) {
      d %>% as.numeric() %>% as_datetime()
    }
    
    start_date <- toDate(input[[plot_brush]]$xmin)
    end_date <- toDate(input[[plot_brush]]$xmax)
    
    return(c(start_date, end_date))
  }
  
  # Calculate average over selection
  calc_average <- function(id) {
    variable <- names(id)$variable
    
    r$data %>%
      u$extract(input[[variable]]) %>%
      u$filter_by_date(get_dates(id)) %>%
      u$get_average_cost() %>%
      return()
  }
  
  # Register a plot + variable + selector input/outputs
  register <- function(id) {
    io <- names(id)
    
    output[[io$plot]] <- renderPlot({
      r$data %>%
        # Extract deviated cost data for given variable
        u$extract(input[[io$variable]]) %>%
        # Call selected plotting function
        p[[input$plot_type]]()
    })

    output[[io$status]] <- renderText({
      dates <- get_dates(id)
      paste0("Start date: ", dates[0], " - ",
             "End date: ", dates[1], " - ",
             "Average cost: ", calc_average(id))
    })
  }
  

  
  register("1")
  register("2")
  avg_cost1 <- reactive({
    calc_average("1")
  })
  avg_cost2 <- reactive({
    calc_average("2")
  })
  
  output$comparison <- renderText({
    paste0("Difference of Average Costs: ", avg_cost1() - avg_cost2())
  })
  
})
