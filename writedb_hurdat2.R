# Code to prepare the HURDAT2 datasets to work with the OISST database
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Source base code -----------------------------------------
source("hurdat2_base.R")

# Create PDI data frame ------------------------------------

# Get hurricanes data frames
hurr.natl.obs <- get_hurr_obs("hurdat2-1851-2016-apr2017.txt") %>%
	mutate(basin = "NATL")
hurr.epac.obs <- get_hurr_obs("hurdat2-nepac-1949-2016-apr2017.txt") %>%
	mutate(basin = "EPAC")
hurr.all.obs <- rbind(hurr.natl.obs, hurr.epac.obs)

# Write into CSV -------------------------------------------

# Whole data set
write_csv(hurr.all.obs, "data/hurdat2-all.csv")

# Filter by available dates in OISST
write_csv(hurr.all.obs %>% dplyr::filter(date.time >= "1981-09-01"),
					"data/hurdat2-1981-2016.csv")

# Testing the CSV files ------------------------------------

# hurr.all.obs.new <- data.table::fread("data/hurdat2-all.csv")

# identical(hurr.all.obs.new$storm.id,   hurr.all.obs$storm.id)
# identical(hurr.all.obs.new$storm.name, hurr.all.obs$storm.name)
# identical(hurr.all.obs.new$n.obs,      hurr.all.obs$n.obs)
# identical(hurr.all.obs.new$record.id,  hurr.all.obs$record.id)
# identical(hurr.all.obs.new$lat,        hurr.all.obs$lat)
# identical(hurr.all.obs.new$long,       hurr.all.obs$long)
# identical(hurr.all.obs.new$storm.year, hurr.all.obs$storm.year)

# Different (a priori) variables

# identical(hurr.all.obs.new$date.time,  hurr.all.obs$date.time)
# identical(hurr.all.obs.new$status,     hurr.all.obs$status)
# identical(hurr.all.obs.new$wind,       hurr.all.obs$wind)
#
# summary(hurr.all.obs.new$wind)
# summary(hurr.all.obs$wind)
