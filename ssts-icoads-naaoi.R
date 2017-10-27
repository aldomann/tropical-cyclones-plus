library(data.table)
library(tidyverse)


# ICOADS 1 jan 1966 data -----------------------------------

icoads <- fread("data/sst-icoads.csv")

icoads <- icoads %>%
	filter(is.na(V4) != TRUE) %>%
	mutate(sst2 = ifelse(V4 == "NaN", NA, as.numeric(V4))) %>%
	mutate(esst = log(30 + sst2)) %>%
	rename(date = V1, lon = V2, lat = V3, sst = V4)


ggplot(icoads) +
	geom_point(aes(x = lon, y = lat, fill = esst))

ggplot(icoads) +
	geom_point(aes(x = lon, y = lat, fill = sst))
