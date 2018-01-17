# R Deviation Cost Explorer

RStudio / Shiny app for exploring SentientSystem deviation analysis data

# Installation

1. After cloning, acquire the deviation data (`converted_data.csv`) and save it under the `data/` folder.

2. Open RStudio

3. Run the following commands in the repl:


```
install.packages(c("devtools", "ggplot2", "tidyverse", "lubridate", "shiny", "shinyTime"))

devtools::install_github('klmr/modules')

```

(This would be easier with automated dependency installation, see issue #12.)

4. Open `global.R` in RStudio

5. Click "Run App" in the top right of the editor window to run the app
