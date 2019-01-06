library(readr)
library(ggplot2)
library(plotly)
library(eurostat)
library(lubridate)
library(dplyr)

input <- list()
input$country <- "AT"

input$year <- 2017

# Download the data from Eurostat
gini <- get_eurostat("ilc_di12")

# Rewrite the date to year
gini$time <- year(gini$time)

# Country subset
gini <- gini %>%
  filter(geo == input$country)

p <- plot_ly(gini, x = ~time, y = ~values, type = "scatter", mode = "lines") %>%
  layout(title = "Gini index over time", 
         xaxis = list(title = "Year"),
         yaxis = list(title = "Gini Index"))

p
