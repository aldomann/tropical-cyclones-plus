# Code to study the PDI dependence with the SST
# Author: Alfredo Hernández <aldomann.designs@gmail.com>


# Libraries ------------------------------------------------

library(data.table)
library(tidyverse)
library(scales)

# ICOADS 1 jan 1966 data -----------------------------------

icoads <- fread("data-tests/icoads-dataset.csv")
icoads <- icoads %>%
	filter(is.na(V4) != TRUE) %>%
	mutate(sst2 = ifelse(V4 == "NaN", NA, as.numeric(V4))) %>%
	mutate(esst = log(30 + sst2)) %>%
	rename(date = V1, lon = V2, lat = V3, sst = V4)

# TODO: the colours aren't being rendered properly
ggplot(icoads, aes(x = lon, y = lat)) +
	geom_tile(aes(fill = sst))
	# geom_point(aes(fill = esst)) +

ggplot(icoads, aes(x = lon, y = lat, colour = sst)) +
	geom_point(size=0.05 ) +
	scale_colour_continuous(low = "#3276fb", high = "#ec3b00",
													space = "Lab", na.value = "#DC066A", guide = "colourbar")
