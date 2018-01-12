# Use import() from kmlr/modules
library(modules)

import_package("shiny", attach = TRUE)
import_package("shinyTime", attach = TRUE)

r <- import("read")
u <- import("util")
p <- import("plot")