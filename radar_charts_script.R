# read the data from RDS in data folder

hh.indicators <- readRDS("./data/indicators_hh.RDS")
ps.indicators <- readRDS("./data/indicators_pers.RDS")

# Transform the data. Mean, Median, qsr80, qsr90 all divided by column max
# Gini and Theil as is
