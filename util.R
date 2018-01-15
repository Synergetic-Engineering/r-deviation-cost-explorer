library(tidyverse)
library(lubridate)

# ------------------------------------------ #
# Utility functions for extracting from data #
# ------------------------------------------ #

# Extract deviation variable table from data
#
# Given data, and a deviated variable (as a character), returns a table
# containing `datetime`, `base_cost`, `deviated_cost`, `deviation_cost` for
# that component.
extract <- function(data, variable) {
  stopifnot(is.data.frame(data))
  stopifnot("value" %in% colnames(data))
  stopifnot("measure" %in% colnames(data))
  stopifnot("step" %in% colnames(data))
  stopifnot("deviated_variable" %in% colnames(data))
  stopifnot(is.character(variable))
  stopifnot(length(variable) == 1)
  
  if(!variable %in% data[["deviated_variable"]]) {
    stop(paste0("Variable not found: ", variable))
  }
  
  COST_MEASURE <- 'blr.comb.c1in.massFlow.cost'
  
  # datetime, base_cost
  base_data <- data %>%
    filter(measure == COST_MEASURE & step == 'base') %>%
    select(datetime, value) %>%
    rename(base_cost = value)
  
  # datetime, deviated_cost
  variable_data <- data %>%
    filter(measure == COST_MEASURE & step == 'deviation' &
             deviated_variable == variable) %>%
    select(datetime, value) %>%
    rename(deviated_cost = value)
  
  # datetime, base_cost, deviated_cost
  base_data %>%
    left_join(variable_data, by = "datetime") %>%
    process_dev() %>%
    arrange(datetime) %>%
    return()
}

# Compute deviation cost for data
#
# Expects table with `base_cost` and `deviated_cost`.
# Returns new table with `deviation_cost`.
# Called by extract()
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

# Get all deviated variable names from data
#
# Given data, returns a vector of unique deviated variable names
get_variable_names <- function(data) {
  stopifnot(is.data.frame(data))
  stopifnot("deviated_variable" %in% colnames(data))
  
  data[["deviated_variable"]] %>%
    na.omit() %>%
    unique() %>%
    return()
}