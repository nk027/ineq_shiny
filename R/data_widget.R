# Widget ------------------------------------------------------------------

# Mean
# Median
# Gini
# QSR
# At-risk-of-poverty Rate
# At-risk-of-poverty Threshold
# Top 10% Share

# IE and CH are only available for 2016

data <- readRDS("data/eurostat.rds")

widget <- list()

widget$mean_inc <- rbind(data$inc_mean %>% 
  filter(time == max(time)) %>% 
  select(country = geo, values),
data$inc_mean %>% 
  filter(geo %in% c("CH", "IE")) %>% 
  filter(time == max(time)) %>% 
  select(country = geo, values))

widget$med_inc <- rbind(data$inc_med %>% 
  filter(time == max(time)) %>% 
  select(country = geo, values),
data$inc_med %>% 
    filter(geo %in% c("CH", "IE")) %>% 
    filter(time == max(time)) %>% 
    select(country = geo, values))

widget$gini <- rbind(data$gini %>% 
  filter(time == max(time)) %>% 
  select(country = geo, values),
data$gini %>% 
  filter(geo %in% c("CH", "IE")) %>% 
  filter(time == max(time)) %>% 
  select(country = geo, values))

widget$qsr <- rbind(data$qsr %>% 
  filter(time == max(time)) %>% 
  select(country = geo, values),
data$qsr %>% 
  filter(geo %in% c("CH", "IE")) %>% 
  filter(time == max(time)) %>% 
  select(country = geo, values))

widget$pov <- rbind(data$pov %>% 
  filter(time == max(time)) %>% 
  select(country = geo, values),
data$pov %>%
  filter(geo %in% c("CH", "IE")) %>% 
  filter(time == max(time)) %>% 
  select(country = geo, values))

widget$pov_th <- rbind(data$pov_th %>% 
  filter(time == max(time)) %>% 
  select(country = geo, values),
data$pov_th %>% 
  filter(geo %in% c("CH", "IE")) %>% 
  filter(time == max(time)) %>% 
  select(country = geo, values))

widget$top10 <- rbind(data$quants %>% 
  filter(time == max(time), quantile == "D10") %>% 
  select(country = geo, values),
data$quants %>% 
  filter(geo %in% c("CH", "IE")) %>% 
  filter(time == max(time), quantile == "D10") %>% 
  select(country = geo, values))

saveRDS(widget, "data/widget.rds")
