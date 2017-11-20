# Base code to study the PDI of hurricane data from the National Hurricane Center (HURDAT)
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

library(tidyverse)
library(stringr) # To split lines
library(lubridate) # Use dates

# Get hurricane observations data frame --------------------

get_hurr_obs <- function(filename){
	# Read and split raw data ----------------------------------

	# tracks.url <- paste0("http://www.aoml.noaa.gov/hrd/hurdat/", "hurdat2-nepac-1949-2016-apr2017.txt")
	# tracks.url <- paste0("http://www.aoml.noaa.gov/hrd/hurdat/", "hurdat2-1851-2016-apr2017.txt")
	tracks.file <- paste0("data/", filename)

	hurr.tracks <- readLines(tracks.file)
	hurr.tracks <- lapply(hurr.tracks, str_split, pattern = ",", simplify = TRUE)

	# Clean the raw data ---------------------------------------

	# Split the hurr.tracks into meta and observation lists
	hurr.lengths <- sapply(hurr.tracks, length)
	hurr.meta <- hurr.tracks[hurr.lengths == 4]
	hurr.obs <- hurr.tracks[hurr.lengths == 21]

	# Create and clean meta data frame
	hurr.meta <- lapply(hurr.meta, tibble::as_tibble)
	hurr.meta <- bind_rows(hurr.meta)

	hurr.meta <- hurr.meta %>%
		dplyr::select(-V4) %>%
		rename(storm.id = V1, storm.name = V2, n.obs = V3) %>%
		mutate(storm.name = str_trim(storm.name),
					 n.obs = as.numeric(n.obs))

	storm.id <- rep(hurr.meta$storm.id, times = hurr.meta$n.obs)

	# Create and clean obs data frame
	hurr.obs <- lapply(hurr.obs, tibble::as_tibble)
	hurr.obs <- bind_rows(hurr.obs) %>%
		mutate(storm.id = storm.id) %>%
		dplyr::select(storm.id, V1:V7) %>%
		rename(date = V1, time = V2, record.id = V3, status = V4, lat = V5, long = V6, wind = V7)

	# Change date and time & unite them
	hurr.obs <- hurr.obs %>%
		unite(date.time, date, time) %>%
		mutate(date.time = ymd_hm(date.time))
	# mutate(decade = substring(year(date.time), 1, 3),
	# decade = paste0(decade, "0s"))

	# Meaningful status names
	storm.levels <- c("TD", "TS", "HU", "EX", "SD", "SS", "LO", "WV", "DB")
	storm.labels <- c("Tropical depression", "Tropical storm", "Hurricane",
										"Extratropical cyclone", "Subtropical depression", "Subtropical storm",
										"Other low", "Tropical wave", "Disturbance")
	hurr.obs <- hurr.obs %>%
		mutate(status = factor(str_trim(status),
													 levels = storm.levels,
													 labels = storm.labels))

	# Clean record identifier
	hurr.obs <- hurr.obs %>%
		mutate(record.id = gsub('\\s+', '', record.id)) %>%
		mutate(record.id = replace(record.id, record.id == "", NA))

	# Morph coordinates
	morph_long <- function(long){
		long <- ifelse(str_extract(long, "[A-Z]") == "W",
									 - as.numeric(str_extract(long, "[^A-Z]+")),
									   as.numeric(str_extract(long, "[^A-Z]+")) )
		return(long)
	}
	morph_lat <- function(lat){
		lat <- ifelse(str_extract(lat, "[A-Z]") == "S",
									- as.numeric(str_extract(lat, "[^A-Z]+")),
									  as.numeric(str_extract(lat, "[^A-Z]+")) )
		return(lat)
	}

	# Split the numeric coordinates from their directions
	hurr.obs <- hurr.obs %>%
		mutate(lat.num = as.numeric(str_extract(lat, "[^A-Z]+")),
					 lat.dir = str_extract(lat, "[A-Z]"),
					 lat = morph_lat(lat),
					 long.num = as.numeric(str_extract(long, "[^A-Z]+")),
					 long.dir = str_extract(long, "[A-Z]"),
					 long = morph_long(long))

	# Clean non-standard data ----------------------------------

	# Ignore data outside the delta.t = 6 hours
	hurr.obs <- hurr.obs %>%
		filter(hour(date.time) == 00 |
					 	hour(date.time) == 06 |
					 	hour(date.time) == 12 |
					 	hour(date.time) == 18) %>%
		filter(minute(date.time) == 00)

	# Clean up wind column -------------------------------------

	# Manually change odd middle values for AL191976 & AL111973
	hurr.obs <- hurr.obs %>%
		mutate(wind = ifelse(storm.id == "AL191976" & wind == " -99", 20, wind),
					 wind = ifelse(storm.id == "AL111973" & wind == " -99", 30, wind),
					 wind = ifelse(storm.id == "AL111973" & month(date.time) == 9 &
					 								day(date.time) == 12 & hour(date.time) == 12, NA, wind)) %>%
		filter(is.na(wind) != TRUE)

	# Clean and reformat the wind column
	hurr.obs <- hurr.obs %>%
		mutate(wind = ifelse(wind == " -99", NA, as.numeric(wind)))

	# Add useful info to data frame ----------------------------

	# Add category 5 hurricanes boolean
	# hurr.obs <- hurr.obs %>%
	# 	group_by(storm.id) %>%
	# 	mutate(cat.5 = max(wind) >= 137) %>%
	# 	ungroup()

	# Add storm.name and storm.year to hurr.obs
	hurr.obs <- hurr.obs %>%
		left_join(hurr.meta, by = "storm.id") %>%
		mutate(storm.year = year(date.time))

	# Recalculate n.obs
	hurr.obs <- hurr.obs %>%
		group_by(storm.id) %>%
		mutate(n.obs = length(wind))

	# Rearrange hurr.obs data frame columns
	hurr.obs <- hurr.obs[c("storm.id", "storm.name", "n.obs", "date.time",
												 "status", "record.id", "lat", "long",
												 "wind", "storm.year")]
	# Unused variables
	# 	"delta.t" after "date.time"
	# 	"cat.5" after "wind"
	#   "decade" after storm.year

	return(hurr.obs)
}
