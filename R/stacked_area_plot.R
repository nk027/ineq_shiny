library(readr)
library(ggplot2)
library(plotly)
library(eurostat)
library(lubridate)
library(dplyr)
library(tidyr)

input <- list()
input$country <- "AT"
# Possible expressions: EUR, PPS, NAC
input$currency <- "EUR"
# Set input quantile, Decile. Possible expressions: "D" for deciles, "QU" for quantiles.
input$quant <- "QU"

# Download the data from Eurostat
deciles <- get_eurostat("ilc_di01")

# Rewrite the date to year
# deciles$time <- year(deciles$time)

# Filter the selected country, currency, and share of total income
deciles <- deciles %>%
  # filter(geo == input$country) %>%
  filter(indic_il == "SHARE", currency == "EUR")

# Filter Quantiles or Deciles
deciles <- deciles %>%
  filter(grepl(input$quant, deciles$quantile))

levels(deciles$quantile) <- droplevels(deciles$quantile)
if (input$quant== "D") {
  deciles$quantile <- ordered(deciles$quantile, levels = c("D10", "D9", "D8", "D7", "D6", "D5", "D4", "D3", "D2", "D1"))
} else {
  deciles$quantile <- ordered(deciles$quantile, levels = c("QU5", "QU4", "QU3", "QU2", "QU1"))
}

saveRDS(deciles, "data/quantiles_stacked.rds")

if (input$quant== "D") {
  input$type <- "deciles"
} else {
  input$type <- "quantiles"
}

# Cumulative stacked plot
p <- ggplot(data = deciles, aes(x = deciles$time, y = deciles$values, fill = factor(deciles$quantile))) +
  geom_area() +
  labs(title = paste0("Income share by ", input$type, "-", input$country))  +
  ylab("Cumulative Share in %") +
  xlab("Year")

ggplotly(p)

