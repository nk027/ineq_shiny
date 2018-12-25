library(readr)
library(ggplot2)
library(plotly)
library(eurostat)
library(lubridate)
library(dplyr)

input <- list()
input$country <- "AT"
# Possible expressions: EUR, PPS, NAC
input$currency <- "PPS"
# Set input quantile, Decile. Possible expressions: "D" for deciles, "QU" for quantiles.
input$quant <- "D"

input$year <- 2017
# Download the data from Eurostat
deciles <- get_eurostat("ilc_di01")

# Rewrite the date to year
deciles$time <- year(deciles$time)

# Download the gross national disposable income
gr.nat.disp.inc <- get_eurostat("teina090")

# Select 2017-01-01 for comparability with income shares
gr.nat.disp.inc <- gr.nat.disp.inc %>%
  filter(time == "2017-01-01")

# Rewrite the date variable
gr.nat.disp.inc$time <- year(gr.nat.disp.inc$time)

# Filter the year and share
deciles <- deciles %>%
  filter(indic_il == "SHARE", currency == "EUR",  time == 2017) 

disp.inc <- gr.nat.disp.inc[, c(6,8)]

# Join gross national disposable income with shares
dat.bar.plot <- left_join(deciles, disp.inc, by = "geo")
# Create variable that shows total disposable income in decile/quantile/quintiles
dat.bar.plot$disp.inc.group <- dat.bar.plot$values.x/100*dat.bar.plot$values.y
dat.bar.plot$diff <- dat.bar.plot$values.y-dat.bar.plot$disp.inc.group

if (input$quant== "D") {
  input$type <- "Deciles"
  dat.bar.plot <- dat.bar.plot %>% 
    filter(quantile == c("D1", "D10"))
} else {
  input$type <- "Quantiles"
  dat.bar.plot <- dat.bar.plot %>% 
    filter(quantile == c("Q1", "Q5"))
}


# Country subset
dat.bar.plot <- dat.bar.plot %>%
  filter(geo == input$country)

# Refactor quantiles
dat.bar.plot$quantile <- droplevels(dat.bar.plot$quantile)
text <- c(paste0("Total income: ", dat.bar.plot$values.y[1]))

p <- plot_ly(dat.bar.plot, type = "bar") %>%
  add_trace(y=~disp.inc.group, name = paste0(input$type, "- income")) %>%
  add_trace(y=~values.y-disp.inc.group, name = "Total income", text = text) %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
p
