x <- read.csv("weights.csv")

x <- as.data.frame(x[, -1])

countrynames <- names(x)

y <- matrix(NA, nrow(x), 5)
for(i in 1:nrow(x)) {
  y[i, ] <- countrynames[which(x[i, ] == 1)]
}

y <- as.data.frame(t(y))
names(y) <- countrynames

saveRDS(y, "data/neighbours.rds")
