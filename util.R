library(tidyverse)
library(lubridate)
library(data.tree)

# ------------------------------------------ #
# Utility functions for extracting from data #
# ------------------------------------------ #

# Extract deviation variable table from data
#
# Args:
#   - data: data.frame    = Contains value, measure, step, deviated_variable
#   - variable: string    = Name of component / deviated variable to extract
#   - var_measure: string = Name of measure for base and deviated variable
#
# Returns a table containing `datetime`, `base_cost`, `deviated_cost`,
# `deviation_cost` for given deviated variable and measure.
#
# extract :: Table -> String -> String -> Table
extract <- function(data, variable, var_measure) {
  stopifnot(is.data.frame(data))
  stopifnot("value" %in% colnames(data))
  stopifnot("measure" %in% colnames(data))
  stopifnot("step" %in% colnames(data))
  stopifnot("deviated_variable" %in% colnames(data))
  stopifnot(is.character(variable))
  stopifnot(length(variable) == 1)
  stopifnot(is.character(var_measure))
  stopifnot(length(var_measure) == 1)
  
  if("unit" %in% colnames(data) & !length(unique(data$unit)) == 1) {
    stop("Can't handle multiple units")
  }
  
  if(!variable %in% data[["deviated_variable"]]) {
    stop(paste0("Variable not found: ", variable))
  }
  
  if(!var_measure %in% data[["measure"]]) {
    stop(paste0("Measure not found: ", var_measure))
  }
  
  BASE_STEP <- 'base'
  DEVIATION_STEP <- 'deviation'
  
  # datetime, base_cost
  base_data <- data %>%
    filter(measure == var_measure & step == BASE_STEP) %>%
    select(datetime, value) %>%
    rename(base_cost = value)
  
  # datetime, deviated_cost
  variable_data <- data %>%
    filter(measure == var_measure & step == DEVIATION_STEP &
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
#
# process_dev :: Table -> Table
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
#
# validate :: Table -> IO ()
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
#
# get_variable_names :: Table -> [String]
get_variable_names <- function(data) {
  stopifnot(is.data.frame(data))
  stopifnot("deviated_variable" %in% colnames(data))
  
  data[["deviated_variable"]] %>%
    na.omit() %>%
    unique() %>%
    return()
}

# Filter by date
#
# Given data and a datetime pair c(date1, date2), remove rows
# which are outside that time.
#
# filter_by_date :: Table -> (Date, Date) -> Table
filter_by_date <- function(variable_data, dates) {
  stopifnot(is.data.frame(variable_data))
  stopifnot("datetime" %in% colnames(variable_data))
  stopifnot(is.timepoint(variable_data$datetime))
  
  return(filter(variable_data, datetime > dates[1] & datetime < dates[2]))
}

# ---------------- #
# Column functions #
# ---------------- #

# Function: get_children_names --> returns the names of the direct decendants for the given node
# get_children_names :: Tree a -> [String]
get_children_names <- function(tree) {
  stopifnot(is(tree, "Node"))
  n <- tree$count
  if(is.null(n))
    return(NULL)
  
  x <- sapply(1:n, function(x, y) y$children[[x]]$name, y = tree)
  x
}