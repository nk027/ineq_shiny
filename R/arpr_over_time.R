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

# PC: Percentage, THS_PER: in thousands of persons
input$value <- "PC"

# Below 60% of median income
input$threshold <- "LI_R_MD60"


# Download the data from Eurostat
arpr <- get_eurostat("ilc_li02")

# Rewrite the date to year
# arpr$time <- year(arpr$time)

# Country subset
arpr <- arpr %>%
  filter(#geo == input$country,
         unit == input$value,
         indic_il == input$threshold,
         age == "TOTAL",
         sex == "T") %>% 
  select(geo, time, values)

saveRDS(arpr, "data/arpr.rds")

p <- plot_ly(arpr, x = ~time, y = ~values, type = "scatter", mode = "lines") %>%
  layout(title = "At risk of poverty rate over time", 
         xaxis = list(title = "Year"),
         yaxis = list(title = "At risk of poverty rate"))

p
