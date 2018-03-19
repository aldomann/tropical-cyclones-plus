# Code to analise the influence of the geographical track of on the duration of hurricanes
# Author: Alfredo Hernández <aldomann.designs@gmail.com>


# Libraries ------------------------------------------------
library(tidyverse)
library(data.table)
library(measurements) # Convert units

# Source base code -----------------------------------------
# source("geographics_base.R")

# Get RAW data ---------------------------------------------

storms.all <- fread('data/hurdat2-all.csv')

storms.natl <- storms.all %>%
	dplyr::filter(basin == "NATL") %>%
	dplyr::filter(storm.year >= 1966)

storms.epac <- storms.all %>%
	dplyr::filter(basin == "EPAC") %>%
	dplyr::filter(storm.year >= 1986)


# Summarise geographical information -----------------------


# Haversine distance

haversine_distance <- function(lat1, lat2, lon1, lon2) {
	earth.radius = 6371000

	lat1 = lat1 * (pi/180)
	lat2 = lat2 * (pi/180)
	lon1 = lon1 * (pi/180)
	lon2 = lon2 * (pi/180)
	delta.lat = lat2 - lat1
	delta.lon = lon2 - lon1

	a <- sin(delta.lat/2) * sin(delta.lat/2) + cos(lat1) * cos(lat2) * sin(delta.lon/2) * sin(delta.lon/2)
	c <- 2 * atan2(sqrt(a), sqrt(1-a))

	return(earth.radius * c)
}

# Summarise data frame

storms.natl <- arrange(storms.natl, date.time)
storms.epac <- arrange(storms.epac, date.time)

storms.test <- storms.natl %>%
	group_by(storm.id) %>%
	mutate(distance = haversine_distance(lat, lag(lat), long, lag(long))) %>%
	mutate(distance = ifelse(is.na(distance), 0, distance)) %>%
	summarise(first.lat = first(lat), last.lat = last(lat),
						first.long = first(long), last.long = last(long),
						distance = sum(distance))
