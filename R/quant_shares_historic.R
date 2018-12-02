# Libraries ----------------------------------------------------------------------
library(plotly)
library(dplyr)
library(readr)
library(plotly)

country <- "CZ"
year <- c(2007:2016)

# Download data --------------------------------------------------------------------------------------------
silc.p <- tbl(pg, "pp") %>%
  filter(pb020 %in% country & pb010 %in% year) %>%
  select(pb010, pb020, pb030, pb040, pb060, pb150, py010g, py020g, py050g, py080g, py090g, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

silc.h <- tbl(pg, "hh") %>%
  filter(hb020 %in% country & hb010 %in% year) %>%
  select(hb010, hb020, hb030, hy010, hy020, hx050, hx090, hy040g, hy050g, hy060g, hy070g, hy080g, hy090g, hy110g, hy120g, hy130g, hy140g) %>%
  collect(n = Inf)

silc.d <- tbl(pg, "dd") %>%
  filter(db020 %in% country & db010 %in% year) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

silc.r <- tbl(pg, "rr") %>% 
  filter(rb020 %in% country & rb010 %in% year) %>%
  select(rb010, rb020, rb030, rx030, rb050) %>%
  collect(n = Inf)

# Save all data ------------------------------------------------------------------------------------------
write_rds(silc.d, "./data/cz_hh_register.RDS")
write_rds(silc.r, "./data/cz_ps_register.RDS")
write_rds(silc.p, "./data/cz_ps_data.RDS")
write_rds(silc.h, "./data/cz_hh_data.RDS")
