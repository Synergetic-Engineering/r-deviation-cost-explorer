library(tidyverse)
library(ggplot2)
library(lubridate)

# TODO: Split into components

# Read data and order by time
data <- read_csv("data/deviation_data.csv") %>%
  arrange(UnixTime) %>%
  mutate(datetime = as_datetime(UnixTime))

#
# Example plot for blr.comb.c1in.massFlow.cost
#
condenser_example <- function() {
  data %>%
    prep_condenser() %>%
    # plot_deviated_baseline()
    # plot_aggregated_dev_cost()
    # plot_bars()
    plot_bars_cmp()
}

# Extract base, deviated, and deviation costs for condenser from data
prep_condenser <- function(data) {
  condenser_data <- data %>%
    rename(
      base_cost = `corp=ambergilbertenergy,site=ambergilbert,unit=unit1,module=deviation/unit,step=base,io=output,measure=blr.comb.c1in.massFlow.cost`,
      condenser_deviated_cost = `corp=ambergilbertenergy,site=ambergilbert,unit=unit1,module=deviation/unit,step=deviation,io=output,deviated_variable=turb.cond.thermalConductance.target,measure=blr.comb.c1in.massFlow.cost`,
      generator_load = `corp=ambergilbertenergy,site=ambergilbert,unit=unit1,module=deviation/unit,step=base,io=input,measure=turb.gen.c2out.energyFlow.use`
    ) %>%
    mutate(
      deviation_cost_balanced = (base_cost - condenser_deviated_cost) / generator_load,
      deviation_cost = base_cost - condenser_deviated_cost,
      agg_dev_cost = cumsum(deviation_cost)
    )
  return(condenser_data)
}

# Plot deviated cost and baseline cost together
plot_deviated_baseline <- function(condenser_data) {
  scale_factor <- 50.0
  shift_factor <- mean(prep_condenser(data)$base_cost, na.rm = TRUE)
  
  ggplot(data = condenser_data) +
    geom_line(mapping = aes(x = datetime, y = condenser_deviated_cost), colour = "red") +
    geom_line(mapping = aes(x = datetime, y = base_cost), colour = "black") +
    geom_line(mapping = aes(x = datetime, y = (deviation_cost * scale_factor) + shift_factor), colour = "blue", alpha = 0.5) +
    scale_y_continuous(sec.axis = ~ (. - shift_factor) / scale_factor) +
    coord_cartesian()
}

# Plot the aggregated deviation cost
plot_aggregated_dev_cost <- function(condenser_data) {
  ggplot(data = condenser_data) +
    geom_line(mapping = aes(x = datetime, y = agg_dev_cost))
}

# Plot a bar chart of the summative deviation cost for each day
plot_bars <- function(condenser_data) {
  bar_data <- condenser_data %>%
    group_by(day = as.Date(datetime)) %>%
    summarise(cost = sum(deviation_cost))
  
  ggplot(data = bar_data) +
    geom_col(mapping = aes(x = day, y = cost)) +
    geom_smooth(mapping = aes(x = day, y = cost), colour = "black", se = FALSE) +
    scale_x_date(date_breaks = "2 days")
}

# Plot a bar chart of the summative deviation cost for each day compared to the average (for each day)
plot_bars_cmp <- function(condenser_data) {
  bar_data <- condenser_data %>%
    group_by(day = as.Date(datetime)) %>%
    summarise(cost = sum(deviation_cost, na.rm = TRUE)) %>%
    mutate(cost_diff = mean(cost, na.rm = TRUE) - cost)
  stopifnot(!is.nan(bar_data$cost_diff))
  

  ggplot(data = bar_data) +
    geom_col(mapping = aes(x = day, y = cost_diff,
                           fill = ifelse(cost_diff > 0, "pos", "neg"))) +
    # Colour positive costs red and negative costs green
    scale_fill_manual(values = c("pos" = "red", "neg" = "green")) +
    guides(fill = "none") +
    scale_x_date(date_breaks = "2 days") +
    labs(x = "day", y = "cost difference", title = "Cost Average Difference vs. Day")
}

