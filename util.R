import_package("tidyverse", attach = TRUE)
import_package("lubridate", attach = TRUE)

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
