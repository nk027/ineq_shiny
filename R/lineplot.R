library(eurostat)
library(ggplot2)
library(plotly)

input <- list()
input$country <- "AT"
neighbours <- data.frame("AT" = c("DE", "IT", "SI", "SK", "CZ", "CH"))

ind_ps <- readRDS("./data/indicators_pers.RDS")

gdp <- get_eurostat("nama_10_pc")
poverty <- get_eurostat("ilc_peps01")


inc_time <- gdp %>%
  filter(geo %in% neighbours[[input$country]]) %>% 
  filter(time > "1999-12-31") %>% 
  filter(unit == "CLV10_EUR_HAB" & na_item == "B1GQ")
inc_country <- gdp %>% 
  filter(geo == input$country) %>% 
  filter(time > "1999-12-31") %>% 
  filter(unit == "CLV10_EUR_HAB" & na_item == "B1GQ")

lineplot <- ggplot(inc_time, aes(x = time, y = values, colour = geo)) +
  geom_line() +
  geom_line(data = inc_country, colour = "#aa2020") +
  scale_colour_grey() +
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
    #panel.border =  element_rect(fill = NA),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = rgb(217, 217, 217, maxColorValue = 255), size = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))

ggplotly(lineplot, tooltip = "text") %>% 
  plotly::config(displaylogo = FALSE, collaborate = FALSE) %>% 
  layout(legend = list(orientation = "v", yanchor = "center", y = 0.6))
