# Libraries ----------------------------------------------------------------------
library(plotly)
library(dplyr)
library(readr)
library(plotly)

country <- "CZ"
year <- c(2007:2017)

# Connect to the PostgreSQL database
if(issue_1_dealt_with) {
  pg <- src_postgres(dbname = "dbname", host = "host",
                     user = "user", password = "password",
                     options = "options")
} else {
  stop("Please deal with Issue #1.")
}

# Download data & Write to RDS --------------------------------------------------------------------------------------------
# silc.p <- tbl(pg, "pp") %>%
#  filter(pb020 %in% country & pb010 %in% year) %>%
#  select(pb010, pb020, pb030, pb040, pb060, pb150, py010g, py020g, py050g, py080g, py090g, py100g, py110g, py120g, py130g, py140g, px030) %>%
#  collect(n = Inf)

# silc.h <- tbl(pg, "hh") %>%
#  filter(hb020 %in% country & hb010 %in% year) %>%
#  select(hb010, hb020, hb030, hy010, hy020, hx050, hx090, hy040g, hy050g, hy060g, hy070g, hy080g, hy090g, hy110g, hy120g, hy130g, hy140g) %>%
#  collect(n = Inf)

# silc.d <- tbl(pg, "dd") %>%
#  filter(db020 %in% country & db010 %in% year) %>%
#  select(db010, db020, db030, db040, db090) %>%
#  collect(n = Inf)

# silc.r <- tbl(pg, "rr") %>% 
#  filter(rb020 %in% country & rb010 %in% year) %>%
#  select(rb010, rb020, rb030, rx030, rb050) %>%
#  collect(n = Inf)

# Download data until 2017, with loops ----------------------------------------------------------------------------------------
silc.r. <- list()
for (i in c("07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17")){
silc.r.[[i]] <-  tbl(pg, paste0("c", i, "r")) %>% 
                 filter(rb020 %in% country & rb010 %in% year) %>%
                 select(rb010, rb020, rb030, rx030, rb050) %>%
                 collect(n = Inf) 
}
 

silc.p. <- list()
for (i in c("07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17")){
  silc.p.[[i]] <-  tbl(pg, paste0("c", i, "p")) %>% 
    filter(pb020 %in% country & pb010 %in% year) %>%
    select(pb010, pb020, pb030, pb040, pb060, pb150, py010g, py020g, py050g, py080g, py090g, py100g, py110g, py120g, py130g, py140g, px030) %>%
    collect(n = Inf) 
}


silc.h. <- list()
for (i in c("07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17")){
  silc.h.[[i]] <-  tbl(pg, paste0("c", i, "h")) %>% 
    filter(hb020 %in% country & hb010 %in% year) %>%
    select(hb010, hb020, hb030, hy010, hy020, hx050, hx090, hy040g, hy050g, hy060g, hy070g, hy080g, hy090g, hy110g, hy120g, hy130g, hy140g) %>%
    collect(n = Inf) 
}

silc.d. <- list()
for (i in c("07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17")){
  silc.d.[[i]] <-  tbl(pg, paste0("c", i, "d")) %>% 
    filter(db020 %in% country & db010 %in% year) %>%
    select(db010, db020, db030, db040, db090) %>%
    collect(n = Inf) 
}

# Creating dataframes -----------------------------------------------------------------------------------

silc.p <- rbind(silc.p.$`07`, silc.p.$`08`, silc.p.$`09`, silc.p.$`10`, silc.p.$`11`, silc.p.$`12`, silc.p.$`13`, silc.p.$`14`, silc.p.$`15`, silc.p.$`16`, silc.p.$`17`)
silc.r <- rbind(silc.r.$`07`, silc.r.$`08`, silc.r.$`09`, silc.r.$`10`, silc.r.$`11`, silc.r.$`12`, silc.r.$`13`, silc.r.$`14`, silc.r.$`15`, silc.r.$`16`, silc.r.$`17`)
silc.h <- rbind(silc.h.$`07`, silc.h.$`08`, silc.h.$`09`, silc.h.$`10`, silc.h.$`11`, silc.h.$`12`, silc.h.$`13`, silc.h.$`14`, silc.h.$`15`, silc.h.$`16`, silc.h.$`17`)
silc.d <- rbind(silc.d.$`07`, silc.d.$`08`, silc.d.$`09`, silc.d.$`10`, silc.d.$`11`, silc.d.$`12`, silc.d.$`13`, silc.d.$`14`, silc.d.$`15`, silc.d.$`16`, silc.d.$`17`)


# Save all data ------------------------------------------------------------------------------------------
write_rds(silc.d, "./data/cz_hh_register.RDS")
write_rds(silc.r, "./data/cz_ps_register.RDS")
write_rds(silc.p, "./data/cz_ps_data.RDS")
write_rds(silc.h, "./data/cz_hh_data.RDS")


# Merging the data files -----------------------------------------------------------------------------------------------

# Merge the Household files. HH-Data and HH-Register
# Create hh-ID in register dataframe
silc.d <- silc.d %>%
  mutate(id_h = paste0(db010, db020, db030))

# Create hh-ID in the household dataframe.
silc.h <- silc.h %>%
  mutate(id_h = paste0(hb010,hb020, hb030))

# merge hh data with hh register to add hh weights
silc.hd <- left_join(silc.h, silc.d %>% select(id_h, db090))


# Merge the personal files. Pers-Data and Pers-Register
# generate personal ID. personal data dataframe
silc.p <- silc.p %>%
  mutate(id_p = paste0(pb010, pb020, pb030))

# generate personal ID. personal register dataframe
silc.r <- silc.r %>%
  mutate(id_p = paste0(rb010, rb020, rb030))

# generate household ID - id_h (merge with country code)
silc.p <- silc.p %>%
  mutate(id_h = paste0(pb010, pb020, px030))


# Merge Personal data and personal register, so all persons are included (also below 16 years of age)
silc.pd <- right_join(silc.p, silc.r %>% select(id_p, rb050))

# Merge household data with personal data. via id_h. All persons remain in Dataframe.
silc.dat <- left_join(silc.pd, silc.hd)

# Save the complete datafile
write_rds(silc.dat, "./data/cz_complete_data.RDS")

# Cleanup
rm(silc.d, silc.h, silc.hd, silc.p, silc.r, silc.pd)

# Data Manipulation --------------------------------------------------------------------------------------------------

# P1 style defined in mail by M. Schnetzer & S. Humer: --------------------------------------------------------------

# Define all household incomes in equivalized household income (divide by equivalized household size)
silc.dat[, grep("hy", colnames(silc.dat))] <- silc.dat[, grep("hy", colnames(silc.dat))]/silc.dat$hx050

# Primary Income (pre-tax factor income) variables
pre.tax.fac <- c("py010g", "py050g", "hy110g", "hy040g", "hy090g", "py080g")

# Creating primary income variable based on the columns specified above. 
silc.dat <- silc.dat %>% 
  mutate(pre.tax.fact.inc = rowSums(silc.dat[, pre.tax.fac], 
                             na.rm = TRUE)) 

# Pre-tax-national income
pre.tax.nat <- c(pre.tax.fac, "py090g", "py100g")

# Creating pre-tax-national income variable based on the columns specified
silc.dat <- silc.dat %>% 
  mutate(pre.tax.nat.inc = rowSums(silc.dat[, pre.tax.nat], 
                                    na.rm = TRUE)) 

# Post-tax disposable income
post.tax.disp <- c(pre.tax.nat, "py110g", "py120g", "py130g", "py140g", "hy050g", "hy060g", "hy070g", "hy080g", "hy120g", "hy130g", "hy140g")

# Defining taxes negatively to build sum correctly
silc.dat[, c("hy120g", "hy130g", "hy140g")] <- silc.dat[, c("hy120g", "hy130g", "hy140g")]*-1

# Creating post-tax-disposable income variable based on the columns specified
silc.dat <- silc.dat %>% 
  mutate(post.tax.disp.inc = rowSums(silc.dat[, post.tax.disp], 
                                   na.rm = TRUE)) 

