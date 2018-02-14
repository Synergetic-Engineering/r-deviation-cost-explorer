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
  # names :: Id -> List
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
  # get_dates :: Id -> (Date, Date)
  get_dates <- function(id) {
    stopifnot(is.character(id))
    stopifnot(length(id) == 1)
    
    toDate <- function(d) {
      d %>% as.numeric() %>% as_datetime()
    }
    
    plot_brush <- names(id)$plot_brush    
    start_date <- toDate(input[[plot_brush]]$xmin)
    end_date <- toDate(input[[plot_brush]]$xmax)
    
    return(c(start_date, end_date))
  }
  
  # Get deviated variable (with d1 suffix) as a string from selectors
  # get_deviated_variable :: Id -> String
  get_deviated_variable <- function(id) {
    stopifnot(is.character(id))
    stopifnot(length(id) == 1)
    
    d1 <- input[[names(id)$selectors$d1]]
    dev_var <- input[[names(id)$selectors$deviated_variable]]
    
    deviated_variable <- paste(d1, dev_var, sep = ".")
    
    return(deviated_variable)
  }
  
  # Get measure as a string from selectors
  # get_measure :: Id -> String
  get_measure <- function(id) {
    stopifnot(is.character(id))
    stopifnot(length(id) == 1)
    
    measure <- input[[names(id)$selectors$measure]]
    
    return(measure)
  }
  
  # Calculate average over selection
  # calc_average :: Id -> Float
  calc_average <- function(id) {
    stopifnot(is.character(id))
    stopifnot(length(id) == 1)
    
    variable <- get_deviated_variable(id)
    measure <- get_measure(id)
    date_range <- get_dates(id)
    
    # Can't give average if we don't know about the data
    if (!(length(variable) > 0 && length(measure) > 0 &&
          length(date_range) > 0)) {
      return(NA)
    }
    
    r$data %>%
      u$extract(variable, measure) %>%
      u$filter_by_date(date_range) %>%
      s$average_cost() %>%
      return()
  }
  
  # Register a plot + variable + selector input/outputs
  # register :: Id -> String -> IO ()
  register <- function(id, color) {
    stopifnot(is.character(id))
    stopifnot(length(id) == 1)
    stopifnot(is.character(color))
    stopifnot(length(color) == 1)
    
    io <- names(id)
    sel <- io$selectors
    
    # Deviated variable dropdown box selection
    output[[sel$unit]] = renderUI({
      selectInput(sel$unit, 'Unit', u$get_children_names(r$tree))
    })
    
    output[[sel$d1]] = renderUI({
      
      if (!is.null(input[[sel$unit]])) {
        selectInput(sel$d1, 'D1 (component)',
                    u$get_children_names(
                      r$tree[[ input[[sel$unit]] ]]))
      }
    })
    
    output[[sel$deviated_variable]] = renderUI({
      if (!is.null(input[[sel$d1]])) {
        selectInput(sel$deviated_variable, 'Deviated Variable',
                    u$get_children_names(
                      r$tree[[ input[[sel$unit]] ]][[ input[[sel$d1]] ]]))
      }
    })
    
    # Plotting
    output[[io$plot]] <- renderPlot({
      # Avoid doing anything until all component info is available
      if (!every(list(get_deviated_variable(id), get_measure(id)), ~ length(.) > 0)) {
        return(NULL);
      }
      
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
  
  # register_measures :: Id -> Id -> IO ()
  register_measures <- function(id1, id2) {
    
    # get_measures :: Id -> Maybe [String]
    get_measures <- function(id) {
      sel <- names(id)$selectors
      if (!is.null(input[[sel$deviated_variable]])) {
        u$get_children_names(r$tree[[ input[[sel$unit]] ]][[ input[[sel$d1]] ]][[ input[[sel$deviated_variable]] ]])
      }
      else {
        return(NULL)
      }
    }
    
    # register_measure_input :: Id -> [String] -> IO ()
    register_measure_input <- function(id, measures) {
      measure_selector <- names(id)$selectors$measure
      dev_var_selector <- names(id)$selectors$deviated_variable
      
      output[[measure_selector]] = renderUI({
        if (!is.null(input[[dev_var_selector]])) {
          selectInput(measure_selector, 'Measure', measures)
        }
      })
    }
    
    measures1 <- get_measures(id1)
    measures2 <- get_measures(id2)
    all_measures <- intersect(measures1, measures2)
    
    register_measure_input(id1, all_measures)
    register_measure_input(id2, all_measures)
  }
  
  # ----------------- #
  # Two Component Tab #
  # ----------------- #
  
  register("1", "red")
  register("2", "blue")
  # register_measures() is dependant on register() calls
  measureObs <- observe({ register_measures("1", "2") })
  
  avg_cost1 <- reactive({
    calc_average("1")
  })
  avg_cost2 <- reactive({
    calc_average("2")
  })
  
  # Plotting for reference plot
  output$reference_plot <- renderPlot({
    variable1 <- get_deviated_variable("1")
    variable2 <- get_deviated_variable("2")
    measure1 <- get_measure("1")
    measure2 <- get_measure("2")
    date_range1 <- get_dates("1")
    date_range2 <- get_dates("2")
    
    
    if (every(list(measure1, measure2, variable1, variable2, date_range1, date_range2),
              ~ length(.) > 0L)) {
      d1 <- r$data %>% u$extract(variable1, measure1) %>% u$filter_by_date(date_range1)
      d2 <- r$data %>% u$extract(variable2, measure2) %>% u$filter_by_date(date_range2)
      plot <- p$plot_two(input$plot_type)
      
      return(plot(d1, d2) + s$aggregated_cost_two_legend(d1, d2))
    }
    else {
      return(p$plot_text("(Waiting for selections)"))
    }
  })
  
  output$comparison <- renderText({
    paste0("Difference of Average Costs: ", avg_cost1() - avg_cost2())
  })
  
  #----------------------#
  # Single Component tab #
  #----------------------#
  
  DUMMY_TIME <- ymd_hms("1970-01-01 00:00:00")
  DUMMY_RANGE <- c(DUMMY_TIME, DUMMY_TIME)
  
  # values_RV :: Reactive (List, Bool, Table)
  values_RV <- reactiveValues(
    va = list(DUMMY_RANGE, DUMMY_RANGE),
    selector_toggle = FALSE,
    data = list(NULL))
  
  # Event updates `values_RV$va` with date ranges based on plot brush
  observeEvent(input[["timeline_brush"]], {
    # Get the dates from the timeline plot
    toDate <- function(d) { d %>% as.numeric() %>% as_datetime() }
    brush <- input[["timeline_brush"]]
    date_range <- c(toDate(brush$xmin), toDate(brush$xmax))
    
    # Append new date range to list of date range selections, keeping the two
    # most recent, and swapping their order every second selection to keep colours
    if (!is.null(brush)) {
      toggle <- values_RV$selector_toggle
      ranges <- values_RV$va
      if (!toggle) {
        ranges <- rev(ranges)
      }
      ranges <- c(list(date_range), ranges)[1:2]
      if (toggle) {
        ranges <- rev(ranges)
      }
      
      values_RV$selector_toggle <- !toggle
      values_RV$va <- ranges
      
    }
  })
  
  # Reactive that updates `values_RV$data` with variable data based on selected variables
  variable_data_RE <- reactive({
    # Extract deviated cost data for FIRST variable
    values_RV$data <- r$data %>%
      u$extract(get_deviated_variable("1"), get_measure("1"))
  })
  
  
  # Timeline plot
  output$timeline_plot <- renderPlot({
    plot <- p$plot_timeline(input$plot_type)

    variable_data_RE()
    
    date_ranges <- values_RV$va
    variable_data <- values_RV$data
    
    # The plot looks nicer when there is no blue rectangle obscuring the selection
    session$resetBrush("timeline_brush")
    
    plot(variable_data, date_ranges[[1]], date_ranges[[2]])
  })
  
  # Comparative plot
  output$single_comparison_plot <- renderPlot({
    d1 <- values_RV$data %>% u$filter_by_date(values_RV$va[[1]])
    d2 <- values_RV$data %>% u$filter_by_date(values_RV$va[[2]])
    plot <- p$plot_two(input$plot_type)
    
    if (nrow(d1) > 0 && nrow(d2) > 0) {
      return(plot(d1, d2) + s$aggregated_cost_two_legend(d1, d2))
    }
    else {
      return(p$plot_text("(Awaiting two timeline selections)"))
    }
  })
  
})
