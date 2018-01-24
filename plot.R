library(modules)
library(ggplot2)

u <- import("util")
validate <- u$validate

# ------------------ #
# Plotting functions #
# ------------------ #

condenser_example <- function() {
  data %>% extract("turb.cond.thermalConductance.target") %>% plot_bars_cmp()
}

# Plot deviation cost
plot_deviation <- function(data, color) {
  validate(data)
  stopifnot(is.character(color))
  
  ggplot(data = data) +
    geom_line(mapping = aes(x = datetime, y = deviation_cost), color = color)
}

# Fill data up to n rows with NAs
# If n is less than the number of rows in the data, just returns the data
fill <- function(variable_data, n) {
  validate(variable_data)
  stopifnot(is.numeric(n))
  
  gap <- n - nrow(variable_data)
  
  if (gap <= 0) {
    return(variable_data)
  }
  else {
    NAs <- rep(NA, times = gap)
    return(reduce(NAs, rbind, .init = variable_data))
  }
}

# Plot deviation cost for two datasets, joined on time
plot_deviation_two <- function(data1, data2) {
  validate(data1)
  validate(data2)
  
  # Fill up data2 with NAs to make it as large as data1
  # or vice verca
  size <- max(nrow(data1), nrow(data2))
  if (nrow(data1) >= nrow(data2)) {
    data2 <- fill(data2, size)
    
    # Use datetime from the larger set
    data1 <- data1 %>%
      select(datetime, deviation_cost) %>%
      rename(deviation_cost.x = deviation_cost)
    data2 <- data2 %>%
      select(deviation_cost) %>%
      rename(deviation_cost.y = deviation_cost)
  }
  else {
    data1 <- fill(data1, size)
    
    # Use datetime from the larger set
    data1 <- data1 %>%
      select(deviation_cost) %>%
      rename(deviation_cost.x = deviation_cost)
    data2 <- data2 %>%
      select(datetime, deviation_cost) %>%
      rename(deviation_cost.y = deviation_cost)
  }
  stopifnot(nrow(data1) == nrow(data2))
  
  combined_data <- bind_cols(data1, data2)
  
  ggplot(data = combined_data) +
    geom_line(mapping = aes(x = datetime, y = deviation_cost.x), colour = "red") +
    geom_line(mapping = aes(x = datetime, y = deviation_cost.y), colour = "blue") +
    labs(y = "deviation cost")
}

# Plot deviated cost and baseline cost together
plot_deviated_baseline <- function(data) {
  validate(data)
  
  scale_factor <- 50.0
  shift_factor <- mean(data$base_cost, na.rm = TRUE)
  
  ggplot(data = data) +
    geom_line(mapping = aes(x = datetime, y = deviated_cost), colour = "red") +
    geom_line(mapping = aes(x = datetime, y = base_cost), colour = "black") +
    geom_line(mapping = aes(x = datetime, y = (deviation_cost * scale_factor) + shift_factor), colour = "blue", alpha = 0.5) +
    scale_y_continuous(sec.axis = ~ (. - shift_factor) / scale_factor) +
    coord_cartesian()
}

# Plot the aggregated deviation cost
plot_aggregated_dev_cost <- function(data) {
  validate(data)
  
  data <- data %>%
    mutate(agg_dev_cost = cumsum(deviation_cost))
  ggplot(data = data) +
    geom_line(mapping = aes(x = datetime, y = agg_dev_cost))
}

# Plot a bar chart of the summative deviation cost for each day
plot_bars <- function(data) {
  validate(data)
  
  bar_data <- data %>%
    group_by(day = as.Date(datetime)) %>%
    summarise(cost = sum(deviation_cost))
  
  ggplot(data = bar_data) +
    geom_col(mapping = aes(x = day, y = cost)) +
    geom_smooth(mapping = aes(x = day, y = cost), colour = "black", se = FALSE) +
    scale_x_date(date_breaks = "2 days")
}

# Plot a bar chart of the summative deviation cost for each day compared to the average (for each day)
plot_bars_cmp <- function(data) {
  validate(data)
  
  bar_data <- data %>%
    group_by(day = floor_date(datetime, "day")) %>%
    summarise(cost = sum(deviation_cost, na.rm = TRUE)) %>%
    mutate(cost_diff = mean(cost, na.rm = TRUE) - cost)
  stopifnot(!is.nan(bar_data$cost_diff))
  
  
  ggplot(data = bar_data) +
    geom_col(mapping = aes(x = day, y = cost_diff,
                           fill = ifelse(cost_diff > 0, "pos", "neg"))) +
    # Colour positive costs red and negative costs green
    scale_fill_manual(values = c("pos" = "red", "neg" = "green")) +
    guides(fill = "none") +
    scale_x_datetime(date_breaks = "2 days", labels=(function(d) { format(d, format = "%Y-%m-%d") })) +
    labs(x = "day", y = "cost difference", title = "Cost Average Difference vs. Day")
}

plot_bars_cmp_two <- function(variable1, variable2, colors = c("red", "blue")) {
  validate(variable1)
  validate(variable2)
  stopifnot(length(colors) == 2 & is.character(colors))
  
  WIDTH <- 0.5
  
  # Group by day and compute cost average difference
  difference <- function(variable) {
    variable %>%
      group_by(day = floor_date(datetime, "day")) %>%
      summarise(cost = sum(deviation_cost, na.rm = TRUE)) %>%
      mutate(cost_diff = mean(cost, na.rm = TRUE) - cost)
  }
  
  variable1 <- variable1 %>%
    difference() %>%
    add_column(variable = "1")
  
  variable2 <- variable2 %>%
    difference() %>%
    add_column(variable = "2")
  
  bar_data <- bind_rows(variable1, variable2)
  
  ggplot(data = bar_data, mapping = aes(day, cost_diff, group = variable)) +
    geom_col(mapping = aes(fill = variable),
             position = "dodge",
             # Must compute width manually else bars do not show up
             width = resolution(as.double(bar_data$day), FALSE) * WIDTH) +
    scale_fill_manual(values = colors) +
    # Suppress legend
    guides(fill = "none") +
    scale_x_datetime(date_breaks = "2 days", labels = partial(format, format = "%Y-%m-%d")) +
    labs(x = "day", y = "cost difference", title = "Cost Average Difference vs. Day")
}

# ------------- #
# Plot Registry #
# ------------- #

# List of plot functions, each with a singular and compartive
# version, for plotting one and two variables, respectively
registry <- list(
  "line_chart" = list(
    "singular" = plot_deviation,
    "comparative" = plot_deviation_two
  ),
  "bar_chart" = list(
    "singular" = plot_bars_cmp,
    "comparative" = plot_bars_cmp_two
  )
)

# Get plotting function of one variable from plot registry
plot_one <- function(plot_name, plot_registry = registry) {
  plot_registry[[plot_name]]$singular
}

# Get plotting function of two variables from plot registry
plot_two <- function(plot_name, plot_registry = registry) {
  plot_registry[[plot_name]]$comparative
}
