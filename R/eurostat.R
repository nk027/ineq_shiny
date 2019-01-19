library(eurostat)
library(dplyr)

names <- c("pov_th", "pov", "quants", "qsr", "gini", 
           "inc", "hlth", "pov_reg")
codes <- c("ilc_li01", "ilc_li02", "ilc_di01", "ilc_di11", "ilc_di12", 
           "ilc_di03", "ilc_lk11", "ilc_li41")

data <- lapply(codes, get_eurostat, stringsAsFactors = FALSE)
names(data) <- names


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
  filter(grepl("D*", quantile), indic_il == "SHARE", currency == "EUR")
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

# extra
data$inc_mean <- data$inc %>% 
  filter(indic_il == "MEI_E")
data$inc_med <- data$inc %>% 
  filter(indic_il == "MED_E")

saveRDS(data, "data/eurostat.rds")

# Map ---------------------------------------------------------------------

shp <- get_eurostat_geospatial(nuts_level = "all")
map <- inner_join(shp, data$pov_reg, by = c("NUTS_ID" = "geo"))

map <- map %>% 
  filter(time == "2016-01-01") %>% 
  select(values, geo)

saveRDS(map, "data/map.rds")
  
# map %>%   
#   plot_ly(name = "Poverty Rate",
#           split = ~geo, color = ~values, 
#           text = ~paste(geo, "\n", values),
#           hoverinfo = "text", hoveron = "fill",
#           alpha = 1, showlegend = FALSE) %>% 
#   layout(title = "Poverty Rate", 
#          xaxis = list(autorange = FALSE, range = c(-10, 30)), 
#          yaxis = list(autorange = FALSE, range = c(32, 70))) %>% 
#   colorbar(title = "in percent", ticksuffix = "%")


# Income ------------------------------------------------------------------

data$inc_mean %>% select(geo, time, values) %>% 
  saveRDS("data/inc_mean.rds")
data$inc_med %>% select(geo, time, values) %>% 
  saveRDS("data/inc_med.rds")

inc_mean_self <- data$inc_mean %>% 
  filter(geo == input$country, time >= "2005-01-01")
inc_med_self <- data$inc_med %>% 
  filter(geo == input$country, time >= "2005-01-01")

inc_mean <- data$inc_mean %>% 
  filter(geo %in% neighbours[[input$country]], time >= "2005-01-01")
inc_med <- data$inc_med %>% 
  filter(geo %in% neighbours[[input$country]], time >= "2005-01-01")

ggplot_inc <- ggplot(inc_mean_self, aes(x = time, colour = geo)) +
  scale_color_manual(values = rev(c("#e4f1f1", "#c6e2e2", "#a4d1d1", "#7bbdbd", "#49a4a4", "#008080"))) +
  geom_ribbon(data = inc_mean,
              aes(ymin = inc_mean$values, ymax = inc_med$values), 
              fill = rgb(0.2, 0.2, 0.2, 0.1)) +
  geom_ribbon(aes(ymin = inc_mean_self$values, ymax = inc_med_self$values),
              colour = "#aa2020", fill = rgb(0, 0, 0, 0)) +
  geom_ribbon(aes(ymin = inc_mean_self$values, ymax = inc_med_self$values), 
              fill = "#aa2020", alpha = 0.2) +
  scale_x_date(expand = c(0, 0),
               labels = scales::date_format("%Y")) +
  scale_y_continuous(labels = scales::number_format(),
                     expand = c(0, 0)) +
  theme(
    axis.title = element_blank(),
    legend.position = "right",
    legend.justification = "center",
    legend.background = element_blank(),
    legend.title = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = rgb(0.85, 0.85, 0.85), size = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))

ggplotly(ggplot_inc, tooltip = "geo") %>% 
  plotly::config(displaylogo = FALSE, collaborate = FALSE) %>% 
  layout(legend = list(orientation = "v", yanchor = "center", y = 0.6))
