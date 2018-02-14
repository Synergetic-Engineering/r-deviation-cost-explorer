library(modules)

u <- import("util")
validate <- u$validate

# ---------------------------------- #
# Variable data statistics functions #
# ---------------------------------- #

#
# N.B. These functions take *variable* data which is assumed to be
# filtered by date, deviated variable, etc. and which represents
# *one* time period with no overlaps.
#

# Calculate the total deviation cost over time
#
# aggregated_cost :: Table -> Float
aggregated_cost <- function(variable_data) {
  validate(variable_data)
  
  sum(variable_data$deviation_cost, na.rm = TRUE)
}

# Get average deviation cost from data
#
# Assume given data has already been filtered by time period and variable.
# Returns average deviation cost for data frame.
#
# average_cost :: Table -> Float
average_cost <- function(variable_data) {
  stopifnot(is.data.frame(variable_data))
  stopifnot("deviation_cost" %in% colnames(variable_data))
  stopifnot(is.numeric(variable_data$deviation_cost))
  
  return(mean(variable_data[["deviation_cost"]], na.rm = TRUE))
}


# --------------------------- #
# Legend generating functions #
# --------------------------- #

# create_stat_legend()
#
# Helper function for creating colour legends which display statistics
#
# Given a function that calculates a statistic from variable data
# and a legend title, returns a function that itself returns a "scale_color_manual()"
# ggproto object which generates a colour legend with that statistic. The returned
# function expects two data frames representing variable data for two variables.
#
# Returned function assumes two geoms with the aes() colour mappings "a" (red)
# and "b" (blue) in that order.
#
# Example usage:
#
# agg_cost <- function(var_data) { sum(var_data$deviation_cost) }
#
# my_custom_legend <- create_stat_legend(agg_cost, "Aggregated Cost")
#
# ggplot(data = combined_data) +
#   geom_line(mapping = aes(..., color = "a")) +
#   geom_line(mapping = aes(..., color = "b")) +
#   my_custom_legend(var_data1, var_data2)
#
# create_stat_legend :: String -> (Table -> String) -> (Table -> Table -> GGProto)
create_stat_legend <- function(title, calc_stat_func) {
  stopifnot(is.character(title) && length(title) == 1)
  stopifnot(is.function(calc_stat_func))
  
  return(
    function(var_data1, var_data2) {
      scale_color_manual(name = title,
                         labels = c(
                           calc_stat_func(var_data1),
                           calc_stat_func(var_data2)
                         ),
                         values = c("a" = "red", "b" = "blue"))
    }
  )
}

# Convert decimal representing dollars to human readable string
# to_dollars :: Float -> String
to_dollars <- function(amount) {
  paste0("$", round(amount))
}

#
# Legend functions below
#
# Inputs: variable_data1, variable_data2
# Returns: ggproto "scale_color_manual"
#

# aggregated_cost_two_legend :: Table -> Table -> GGProto
aggregated_cost_two_legend <- create_stat_legend("Aggregated Cost",
  compose(to_dollars, aggregated_cost))

# average_cost_two_legend :: Table -> Table -> GGProto
average_cost_two_legend <- create_stat_legend("Average Cost",
  compose(to_dollars, average_cost))

# TODO: Some sort of dummy stat which maybe hides the label?

