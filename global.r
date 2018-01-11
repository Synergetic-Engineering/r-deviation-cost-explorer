library(shiny)
library(shinyTime)

library(tidyverse)
library(ggplot2)
library(lubridate)

# --------------------- #
# Read and process data #
# --------------------- #

# Read data and order by time
data <- read_csv("data/deviation_data.csv") %>%
  arrange(UnixTime) %>%
  mutate(datetime = as_datetime(UnixTime))

# ------------------------------------------ #
# Utility functions for extracting from data #
# ------------------------------------------ #

# Extract deviation variable table from data
#
# Given data, and a deviated variable (as a character), returns a table
# containing `datetime`, `base_cost`, and `deviated_cost` for that component.
extract <- function(data, variable) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(variable))
  stopifnot(length(variable) == 1)
  
  base_cost_col <- "corp=ambergilbertenergy,site=ambergilbert,unit=unit1,module=deviation/unit,step=base,io=output,measure=blr.comb.c1in.massFlow.cost"
  
  # Find deviated cost column for given variable
  var_col_regex <- str_c("deviated_variable=", variable, ",measure=blr\\.comb\\.c1in\\.massFlow\\.cost$")
  idx <- str_which(colnames(data), var_col_regex)
  if (length(idx) == 0) {
    stop(str_c("No such variable found \"", variable, "\""))
  }
  if (length(idx) > 1) {
    stop(str_c("Multiple matches found for \"", variable, "\": ", str_c(idx, collapse = ", ")))
  }
  deviated_cost_col <- colnames(data)[idx]
  
  data %>%
    select(datetime, base_cost = !!base_cost_col, deviated_cost = !!deviated_cost_col) %>%
    return()
}

# Compute deviation cost for data
#
# Expects table with `base_cost` and `deviated_cost`.
# Returns new table with `deviation_cost`.
process_dev <- function(variable_data) {
  stopifnot(is.data.frame(variable_data))
  stopifnot("base_cost" %in% colnames(variable_data))
  stopifnot(is.numeric(variable_data$base_cost))
  stopifnot("deviated_cost" %in% colnames(variable_data))
  stopifnot(is.numeric(variable_data$deviated_cost))
  
  variable_data %>%
    mutate(deviation_cost = base_cost - deviated_cost) %>%
    return()
}

# Validate processed deviated variable data
#
# Throw an error if table does not contain `base_cost`, `deviated_cost`,
# `deviation_cost`, or `datetime`. Returns NULL.
validate <- function(variable_data) {
  stopifnot(is.data.frame(variable_data))
  
  stopifnot("base_cost" %in% colnames(variable_data))
  stopifnot(is.numeric(variable_data$base_cost))
  stopifnot("deviated_cost" %in% colnames(variable_data))
  stopifnot(is.numeric(variable_data$deviated_cost))
  stopifnot("deviation_cost" %in% colnames(variable_data))
  stopifnot(is.numeric(variable_data$deviation_cost))
  stopifnot("datetime" %in% colnames(variable_data))
  stopifnot(is.timepoint(variable_data$datetime))
  
}

# ------------------ #
# Plotting functions #
# ------------------ #

# Plot a bar chart of the summative deviation cost for each day compared to the average (for each day)
plot_bars_cmp <- function(data) {
  validate(data)
  
  bar_data <- data %>%
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