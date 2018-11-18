# Libraries
library(plotly)
library(dplyr)
library(readr)
library(plotly)

# read the data from RDS in data folder
hh.indicators <- readRDS("./data/indicators_hh.RDS")
ps.indicators <- readRDS("./data/indicators_pers.RDS")

# Transform the data. Mean, Median, qsr80, qsr90 all divided by column max
# Gini and Theil as is
hh.indicators[, 3] <- hh.indicators[, 3]/max(hh.indicators[, 3])
hh.indicators[, 4] <- hh.indicators[, 4]/max(hh.indicators[, 4])
hh.indicators[, 5] <- hh.indicators[, 5]/max(hh.indicators[, 5])
hh.indicators[, 6] <- hh.indicators[, 6]/max(hh.indicators[, 6])

# The same for the personal file
ps.indicators[, 3] <- ps.indicators[, 3]/max(ps.indicators[, 3])
ps.indicators[, 4] <- ps.indicators[, 4]/max(ps.indicators[, 4])
ps.indicators[, 5] <- ps.indicators[, 5]/max(ps.indicators[, 5])
ps.indicators[, 6] <- ps.indicators[, 6]/max(ps.indicators[, 6])

# Select the country to create the plots
country <- "SE"

p <- plot_ly(
  type = 'scatterpolar', 
  r = as.double(ps.indicators[which(hh.indicators$country == country), 2:7]), # Apparently, the numerics need to be double =)
  theta = c('gini', 'mean', 'median', 'qsr80', 'qsr90', 'theil'),
  fill = 'toself'
) %>% 
  layout(
    polar = list(
      radialaxis = list(
        visible = TRUE, 
        range= c(0, 1)
      )
    ), 
    showlegend = FALSE
  )

p

