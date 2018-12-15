library(eurostat)
library(dplyr)

names <- c("pov_th", "pov", "quants", "qsr", "gini", 
           "inc", "hlth", "pov_reg")
codes <- c("ilc_li01", "ilc_li02", "ilc_di01", "ilc_di11", "ilc_di12", 
           "ilc_di13", "ilc_lk11", "ilc_li41")

data <- lapply(codes, get_eurostat, stringsAsFactors = FALSE)
names(data) <- names


# Widget ------------------------------------------------------------------

# Mean
# Median
# Gini
# QSR
# At-risk-of-poverty Threshold
# At-risk-of-poverty Rate
# Top 10% Share
# Health


# Plots -------------------------------------------------------------------

# poverty risk over time
# income quantiles stacked over time
# qsr stacked barchart over time
# inc mean vs. median over time
# nuts2 poverty map


# Prep --------------------------------------------------------------------

data$pov_th <- data$pov_th %>% 
  # single hh, 60% of median eqivalised
  filter(hhtyp == "A1", indic_il == "LI_C_MD60", currency == "EUR")
data$pov <- data$pov %>%
  # percentage, 60% of median equivalised after transfers
  filter(unit == "PC", indic_il == "LI_R_MD60", sex == "T", age == "TOTAL")
data$quants <- data$quants %>% 
  # deciles, share of national equivalised income
  filter(grepl("D[1-10]$", quantile), indic_il == "SHARE", currency == "EUR")
data$qsr <- data$qsr %>% 
  filter(sex == "T", age == "TOTAL")
data$gini <- data$gini %>% 
  # equivalised disposable income after transfers
  filter()
data$inc <- data$inc %>% 
  # median and mean equivalised net income
  filter(unit == "EUR", sex == "T", age == "TOTAL")
data$hlth <- data$hlth %>% 
  # median
  filter(quantile == "QU5", age == "TOTAL")
data$pov_reg <- data$pov_reg %>% 
  # percentage
  filter(unit == "PC")


# Map ---------------------------------------------------------------------

shp <- get_eurostat_geospatial(nuts_level = "all")
map <- inner_join(shp, data$pov_reg, by = c("NUTS_ID" = "geo"))

map %>% 
  filter(time == max(map$time)) %>% 
  select(values) %>% 
  plot(pal = viridis::viridis_pal(), xlim = c(-8, 32), ylim = c(32, 72), 
       main = "Poverty Rates")

