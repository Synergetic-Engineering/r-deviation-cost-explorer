library(ggplot2)

u <- import("util")
validate <- u$validate

# ------------------ #
# Plotting functions #
# ------------------ #

condenser_example <- function() {
  data %>% extract("turb.cond.thermalConductance.target") %>% plot_bars_cmp()
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
