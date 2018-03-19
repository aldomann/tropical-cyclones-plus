# Code to analise the influence of the geographical track of on the duration of hurricanes
# Author: Alfredo Hernández <aldomann.designs@gmail.com>


# Libraries ------------------------------------------------
library(measurements) # Convert units

# Source base code -----------------------------------------
source("geographic_base.R")

# Get RAW data ---------------------------------------------

storms.all <- fread('data/hurdat2-all.csv')

storms.natl <- storms.all %>%
	dplyr::filter(basin == "NATL") %>%
	dplyr::filter(storm.year >= 1966)

storms.epac <- storms.all %>%
	dplyr::filter(basin == "EPAC") %>%
	dplyr::filter(storm.year >= 1986)

storms.natl <- arrange(storms.natl, date.time)
storms.epac <- arrange(storms.epac, date.time)

storms.all <- rbind(storms.natl, storms.epac)


# Summarise geographical information -----------------------

# Summarise data frame

storms.tracks <- storms.all %>%
	group_by(storm.id) %>%
	mutate(distance = haversine_distance(lat, lag(lat), long, lag(long))) %>%
	mutate(distance = ifelse(is.na(distance), 0, distance)) %>%
	summarise(first.lat = first(lat), last.lat = last(lat),
						first.long = first(long), last.long = last(long),
						distance = sum(distance))


# Read PDI data frame

pdi.all <- fread('data/hurdat2-hadisst-1966-2016_pdis.csv') %>%
	mutate(storm.duration = conv_unit(storm.duration, "sec", "hr"))

# Join data frames by storm.id
storms.joint <- full_join(pdi.all, storms.tracks)

storms.joint <- storms.joint %>%
	dplyr::filter(!is.na(storm.name))
