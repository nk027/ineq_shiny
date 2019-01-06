library(readr)
library(ggplot2)
library(plotly)
library(eurostat)
library(lubridate)
library(dplyr)

input <- list()
input$country <- "AT"

# Possible expressions: EUR, PPS, NAC
input$currency <- "EUR"

# Download the data from Eurostat
quintiles <- get_eurostat("ilc_di01")

quintiles$time <- year(quintiles$time)

quintiles <- quintiles %>%
  filter(geo == input$country, indic_il == "TC", currency == input$currency, time >2002)

quintiles <- quintiles[which(quintiles$quantile == "QU1" |
                             quintiles$quantile == "QU2" | 
                             quintiles$quantile == "QU3" | 
                             quintiles$quantile == "QU4" |
                             quintiles$quantile == "P99"), ]

quintiles$quantile <- droplevels(quintiles$quantile)

p <- plot_ly(quintiles, x = ~time, y = ~values) %>%
  add_lines(color = ~quantile) %>%
  layout(title = "Max income in group over time", 
         xaxis = list(title = "Year"),
         yaxis = list(title = "Income in Euro"))
  

p
