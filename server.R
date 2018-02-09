server <- shinyServer(function(input, output, session) {
  
  # Get the input/output names of a plot
  #
  # Return input/output names of a given plot as a list with
  # plot, plot_brush, variable, status and input selectors.
  # Given an id.
  # 
  # Example usage:
  # output[[names(1)$plot]]
  #
  names <- function(id) {
    stopifnot(is.character(id))
    stopifnot(length(id) == 1)
    
    name_of <- function(el) { paste0(el, id) }
    return(list(plot = name_of("plot"),
                plot_brush = name_of("plot_brush"),
                variable = name_of("variable"),
                status = name_of("status"),
                
                selectors = list(
                  unit = name_of("unit"),
                  d1 = name_of("d1"),
                  deviated_variable = name_of("deviated_variable"),
                  measure = name_of("measure")
                )
            )
    )
  }
  
  # Get date range as a vector pair: c(<start>, <end>)
  get_dates <- function(id) {
    stopifnot(is.character(id))
    stopifnot(length(id) == 1)
    
    plot_brush <- names(id)$plot_brush
    toDate <- function(d) {
      d %>% as.numeric() %>% as_datetime()
    }
    
    start_date <- toDate(input[[plot_brush]]$xmin)
    end_date <- toDate(input[[plot_brush]]$xmax)
    
    return(c(start_date, end_date))
  }
  
  # Get deviated variable (with d1 suffix) as a string from selectors
  get_deviated_variable <- function(id) {
    stopifnot(is.character(id))
    stopifnot(length(id) == 1)
    
    d1 <- input[[names(id)$selectors$d1]]
    dev_var <- input[[names(id)$selectors$deviated_variable]]
    
    deviated_variable <- paste(d1, dev_var, sep = ".")
    
    return(deviated_variable)
  }
  
  # Get measure as a string from selectors
  get_measure <- function(id) {
    stopifnot(is.character(id))
    stopifnot(length(id) == 1)
    
    measure <- input[[names(id)$selectors$measure]]
    
    return(measure)
  }
  
  # Calculate average over selection
  calc_average <- function(id) {
    stopifnot(is.character(id))
    stopifnot(length(id) == 1)
    
    variable <- get_deviated_variable(id)
    measure <- get_measure(id)
    
    r$data %>%
      u$extract(variable, measure) %>%
      u$filter_by_date(get_dates(id)) %>%
      u$get_average_cost() %>%
      return()
  }
  
  # Register a plot + variable + selector input/outputs
  register <- function(id, color) {
    stopifnot(is.character(id))
    stopifnot(length(id) == 1)
    stopifnot(is.character(color))
    stopifnot(length(color) == 1)
    
    io <- names(id)
    sel <- io$selectors
    
    # Deviated variable dropdown box selection
    output[[sel$unit]] = renderUI({
      selectInput(sel$unit, 'Unit', u$get_childen_names(r$tree))
    })
    
    output[[sel$d1]] = renderUI({
      message("renderUI:d1")
      
      if (!is.null(input[[sel$unit]])) {
        selectInput(sel$d1, 'D1 (component)',
                    u$get_childen_names(
                      r$tree[[ input[[sel$unit]] ]]))
      }
    })
    
    output[[sel$deviated_variable]] = renderUI({
      message("renderUI:deviated variable")
      if (!is.null(input[[sel$d1]])) {
        selectInput(sel$deviated_variable, 'Deviated Variable',
                    u$get_childen_names(
                      r$tree[[ input[[sel$unit]] ]][[ input[[sel$d1]] ]]))
      }
    })
    
    output[[sel$measure]] = renderUI({
      message("renderUI:measure")
      if (!is.null(input[[sel$deviated_variable]])) {
        selectInput(sel$measure, 'Measure',
                    u$get_childen_names(
                      r$tree[[ input[[sel$unit]] ]][[ input[[sel$d1]] ]][[ input[[sel$deviated_variable]] ]]))
      }
    })
    
    # Plotting
    output[[io$plot]] <- renderPlot({
      # Extract deviated cost data for given variable
      variable_data <- r$data %>%
        u$extract(get_deviated_variable(id), get_measure(id))
      
      # Call chosen plotting function from input
      plot <- p$plot_one(input$plot_type)
      tryCatch({
        plot(variable_data, color)
      },
      error = function(e) {
        # Try without color
        plot(variable_data)
      })

    })

    # Statistics
    output[[io$status]] <- renderText({
      dates <- get_dates(id)
      paste0("Start date: ", dates[1], " - ",
             "End date: ", dates[2], " - ",
             "Average cost: ", calc_average(id))
    })
  }
  

  
  register("1", "red")
  register("2", "blue")
  avg_cost1 <- reactive({
    calc_average("1")
  })
  avg_cost2 <- reactive({
    calc_average("2")
  })
  
  output$reference_plot <- renderPlot({
    variable1 <- get_deviated_variable("1")
    variable2 <- get_deviated_variable("2")
    measure1 <- get_measure("1")
    measure2 <- get_measure("2")
    d1 <- r$data %>% u$extract(variable1, measure1) %>% u$filter_by_date(get_dates("1"))
    d2 <- r$data %>% u$extract(variable2, measure2) %>% u$filter_by_date(get_dates("2"))
    plot <- p$plot_two(input$plot_type)
    
    plot(d1, d2)
  })
  
  output$comparison <- renderText({
    paste0("Difference of Average Costs: ", avg_cost1() - avg_cost2())
  })
  
})
