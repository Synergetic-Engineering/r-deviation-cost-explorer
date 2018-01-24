library(tidyverse)
library(lubridate)

# ------------------- #
# Read converted data #
# --------------------#

# Read expanded data with no mangled columns (not ordered)
data <- read_csv("data/converted_data.csv")

# ------------------------------- #
# Read, process, and convert data #
# ------------------------------- #

# Unpack a column
#
# Given a data frame containing a single column with a mangled header such as
# "corp=ambergilbertenergy,site=ambergilbert,unit=unit1,...", returns a data
# frame with each key-value pair in its own column where the key is the column
# name and the value is repeated across every row. Renames the original column
# to "value".
unpack <- function(col) {
  if(!is.data.frame(col)) stop("col must be a data frame")
  if(!length(col) == 1) stop("Single column only")
  if(!str_detect(colnames(col)[1], ",")) stop("Column name not unpackable")
  
  header <- colnames(col)[1]
  
  # List of c(key, value) pairs
  keyvalues <- header %>%
    str_split(",") %>%
    unlist() %>%
    str_split("=")
  
  # Returns table with extra column named <key> with repeated <value> to table
  # given a c(<key>, <value>) pair
  add_col <- function(table, keyvalue) {
    add_column(table, !!keyvalue[1] := !!keyvalue[2])
  }
  
  unpacked_table <- keyvalues %>%
    reduce(add_col, .init = col) %>%
    rename(value = !!header) %>%
    mutate(value = as.numeric(value))
  
  return(unpacked_table)
}

# Expand entire data frame
#
# Unpack() cost-related columns in the data so that they are tidy and
# header names contain a single value.
expand <- function(data) {
  stopifnot(is.data.frame(data))
  stopifnot("datetime" %in% colnames(data))
  
  colnames(data) %>%
    keep(~ str_detect(.x, ",")) %>% # Remove `datetime`, `UnixTime` column names
    map(function(x) select(data, !!x)) %>% # Convert column names to data frames containing that column
    map(unpack) %>% # Unpack each single-column data frame into a multi-column one
    map(function(x) bind_cols(x, select(data, datetime))) %>% # Add back a datetime column to each
    bind_rows() %>% # Stack rows from all data frames into a single table
    return()
}

# Clean and convert raw data (not pure!)
#
# Read data from specified csv file, convert it using expand(), and save it
convert <- function(data_csv) {
  stopifnot(is.character(data_csv))
  
  data <- read_csv(data_csv) %>%
    mutate(datetime = as_datetime(UnixTime)) %>%
    expand()
  
  write.csv(data, file = "data/converted_data.csv", row.names = FALSE)
}