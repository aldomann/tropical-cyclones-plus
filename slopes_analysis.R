# Code to perform different statistical tests to analise the slopes of PDI vs duration
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Libraries ------------------------------------------------
library(tidyverse)
library(lubridate)
library(data.table)
library(measurements) # Convert units

# Get RAW data ---------------------------------------------

pdi.all <- fread('data/hurdat2-hadisst-1966-2016.csv')
pdi.natl <- pdi.all %>%
	dplyr::filter(basin == "NATL")
pdi.epac <- pdi.all %>%
	dplyr::filter(basin == "epac")

# Clean data -----------------------------------------------

# North Atlantic
pdi.natl.low <- pdi.natl %>%
	dplyr::filter(sst.class == "low")

pdi.natl.high <- pdi.natl %>%
	dplyr::filter(sst.class == "high")

# East Pacific
pdi.epac.low <- pdi.epac %>%
	dplyr::filter(sst.class == "low")

pdi.epac.high <- pdi.epac %>%
	dplyr::filter(sst.class == "high")

# Test plot ------------------------------------------------

ggplot() +
	aes(x = storm.duration, y = storm.pdi) +
	geom_point(data = pdi.natl.low, aes(colour = 'low')) +
	geom_point(data = pdi.natl.high, aes(colour = 'high')) +
	scale_x_log10() +
	scale_y_log10()
