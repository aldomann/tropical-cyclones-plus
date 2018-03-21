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


# Windows of activity
# years.natl <- 1966:2016
coords.natl <- c("90W", "20W", "5N", "25N")
coords.natl.map <- c("100W", "0E", "0N", "60N")

# years.epac <- 1966:2016
coords.epac <- c("120W", "90W", "5N", "20N")
coords.epac.map <- c("160W", "90W", "5N", "35N")

# Maps of the basins (full)
map_region_hurrs(storms.natl, coords.natl.map, coords.natl, steps = c(20, 10), xtra.lims = c(3,2)) #+ theme_bw() + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "map-natl.pdf", width = 5.75, height = 3.75, dpi = 96, device = cairo_pdf)
map_region_hurrs(storms.epac, coords.epac.map, coords.epac, steps = c(10, 10), xtra.lims = c(3,2)) #+ theme_bw() + theme(text = element_text(family = "LM Roman 10")) + ggsave(filename = "map-epac.pdf", width = 6, height = 3.15, dpi = 96, device = cairo_pdf)


# Analysis of travelled distance ---------------------------

storms.joint %>%
	group_by(basin) %>%
	summarise(dist.mean = mean(distance))

get_longest_paths("NATL")
get_longest_paths("EPAC")

plot_distance_scatterplot(storms.joint) + theme_bw()
