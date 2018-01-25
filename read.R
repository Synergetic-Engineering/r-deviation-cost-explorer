library(tidyverse)
library(lubridate)
library(data.tree)
library(treemap)
library(yaml)

# ------------------------------- #
# Read, process, and convert data #
# ------------------------------- #

# Regenerate data (WARNING: May take a long time!)
regenerate <- function(data_csv = "data/deviation_data.csv") {
  # Decompose columns
  decompose_columns(data_csv, "data/decomposition.csv")
  
  # Column yaml file
  column_data <- read_csv("data/decomposition.csv")
  tree <- to_tree(column_data)
  write_tree(tree, "data/columns.yaml")
  
  # Expand data
  convert(data_csv)
}

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

# ------------------ #
# Parse column names #
# ------------------ #

# Function: parse_measure 
# Purpose: decompose measure or deviated_variable into elements  
# input: 
#   df: data frame with 2 columns: names & values
#   meas_name: name of the measurement to be decomposed
# return: data frame if measurement is found or NA
parse_measure <- function(df, name = "measure", prefix = "m") {
  m <- df$values[df$names == name]
  if(!identical(m, character(0))){
    values <- unlist(strsplit(x = m, "\\."))
    names <- sapply(1:length(values), function(x) paste0(prefix, "_", x))
    data.frame(names, values, stringsAsFactors = FALSE)
  } else {
    NULL
  } 
}

# parse_column_name: Decomposes an aggregated column name into a data frame containing the various elements (with names) 
# Inputs: 
#   full_name: original aggregated column name (e.g "corp=amber,site=ag,unit=unit1,module=dev/unit,step=base,io=output,measure=blr.comb.c1in.massFlow.cost)  
#   new_name: new column name of returned data frame
# Output: returns data frame with column of variables names and another column with their values 
# Notes: the measure is also decomposed
parse_column_name <- function(col_name) {
  # split up parts separated by commas, then bits separated by '='
  parts <- unlist(strsplit(x = col_name, ","))
  bits <- apply(as.array(parts), 1, function(x) unlist(strsplit(x, "=")))
  
  # construct as data frame 
  names  <- apply(bits, 2, function(x) x[1])
  values <- apply(bits, 2, function(x) x[2])
  df <- data.frame (names, values, stringsAsFactors = FALSE)
  
  # add decomposition for both measure and deviated_variable (where applicable), then transpose and convert back into data frame 
  df <- df %>% bind_rows(parse_measure(., "deviated_variable"          , "d")) %>%
    bind_rows(parse_measure(., "measure"                    , "m")) %>% 
    t() %>% data.frame(stringsAsFactors = FALSE)
  
  # add names to existing data frame
  colnames(df) <- df["names",]      # add column names 
  df <- df[-1,]               # drop first row (containing column names)
  df
}

# Extract just the column names from the data and write to a csv (not pure!)
decompose_columns <- function(data_csv = "data/deviation_data.csv", output_file = "data/decomposition.csv") {
  # read deviation data from file & extract list of column names
  dev_data <- read.csv2(data_csv, sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE) 
  col_names <- (colnames(dev_data))[-1]
  
  col_names %>% sapply(function(x) parse_column_name(x)) %>% bind_rows() %>% write.csv(output_file, row.names = FALSE)
}

# ---------------- #
# Columns as Trees #
# ---------------- #

# Convert columns to a tree
to_tree <- function(columns) {
  stopifnot(is.data.frame(columns))
  
  columns <- columns %>%
    select(unit, d_1, deviated_variable, measure) %>%
    # Remove "base" rows
    filter(!is.na(deviated_variable)) %>%
    # Remove prefix from dev var names e.g. "turb.one" -> "turb"
    mutate(stripped_dev_var = str_replace(deviated_variable, "^.*?\\.", ""))
  
  if (nrow(columns) == 0) {
    stop("Can't make tree: 0 rows after filtering")
  }
  
  columns$pathString <- paste("market",
                            columns$unit,
                            columns$d_1,
                            columns$stripped_dev_var,
                            columns$measure,
                            sep = "/")

  as.Node(columns)
}

# Write a column-tree as a yaml file
write_tree <- function(tree, filename = "data/columns.yaml") {
  stopifnot(isRoot(tree))
  
  write_yaml(ToListSimple(tree), filename)
}

# Read a column-tree from a yaml file
read_tree <- function(filename = "data/columns.yaml") {
  as.Node(yaml.load_file(filename))
}

# ------------------- #
# Read converted data #
# --------------------#

data <- read_csv("data/converted_data.csv")
column_data <- read_csv("data/decomposition.csv")
tree <- read_tree("data/columns.yaml")
