# Widget ------------------------------------------------------------------

# Mean
# Median
# Gini
# QSR
# At-risk-of-poverty Rate
# At-risk-of-poverty Threshold
# Top 10% Share

data <- readRDS("data/eurostat.rds")

widget <- list()

widget$mean_inc <- data$inc_mean %>% 
  filter(time == max(time)) %>% 
  select(country = geo, values)

widget$med_inc <- data$inc_med %>% 
  filter(time == max(time)) %>% 
  select(country = geo, values)

widget$gini <- data$gini %>% 
  filter(time == max(time)) %>% 
  select(country = geo, values)

widget$qsr <- data$qsr %>% 
  filter(time == max(time)) %>% 
  select(country = geo, values)

widget$pov <- data$pov %>% 
  filter(time == max(time)) %>% 
  select(country = geo, values)

widget$pov_th <- data$pov_th %>% 
  filter(time == max(time)) %>% 
  select(country = geo, values)

widget$top10 <- data$quants %>% 
  filter(time == max(time), quantile == "D10") %>% 
  select(country = geo, values)

saveRDS(widget, "data/widget.rds")
