import_package("tidyverse", attach = TRUE)
import_package("lubridate", attach = TRUE)

# --------------------- #
# Read and process data #
# --------------------- #

# Read data and order by time
data <- read_csv("data/deviation_data.csv") %>%
  arrange(UnixTime) %>%
  mutate(datetime = as_datetime(UnixTime))