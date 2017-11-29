# Base code to populate the HURDAT2 data with SST data from OISST
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

library(tidyverse)
library(lubridate)
library(raster)
library(ncdf4)
library(data.table)

# Source OISST Function ------------------------------------

# SRC: https://github.com/millerlp/Misc_R_scripts/blob/master/NOAA_OISST_ncdf4.R
source("NOAA_OISST_ncdf4.R")
mask <- "data/lsmask.oisst.v2.nc"

# Functions ------------------------------------------------

get_oisst_by_date <- function(date, long, lat){
	file.id = paste0(year(date),
									 sprintf("%02d", month(date)),
									 sprintf("%02d", day(date)))
	folder.id = substr(file.id, 1, 6)

	oisst.file = paste0("oisst-data/avhrr-only-v2.", file.id, ".nc")
	if( !file.exists(oisst.file) ) {
		download.file(paste0("https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/access/avhrr-only/", folder.id, "/avhrr-only-v2.", file.id, ".nc"), oisst.file)
	}

	sst <- extractOISST1day(oisst.file, mask, long, long, lat, lat)
	return(sst)
}

populate_sst <- function(hurr.all.obs){
	hurr.all.obs <- hurr.all.obs %>%
		rowwise() %>%
		mutate(sst = get_oisst_by_date(date.time, ifelse(long < 0, 360+long , long), lat))

	return(hurr.all.obs)
}

# Mine SST data from OISST ---------------------------------

if (file.exists("data/hurdat2-1981-2016.csv") & !file.exists("oisst-data/download_oisst_data.sh")) {
	system("awk -f get_oisst_urls.awk data/hurdat2-1981-2016.csv > oisst-data/download_oisst_data.sh")
	# Total time: 16h 45min
}

if (file.exists("data/hurdat2-1981-2016.csv") & !file.exists("data/hurdat2-oisst-1981-2016.csv")) {
	hurr.all.obs <- fread("data/hurdat2-1981-2016.csv")

	hurr.all.obs.new <- hurr.all.obs %>%
		mutate(date.time = ymd_hms(date.time))

	hurr.all.obs.full <- populate_sst(hurr.all.obs.new)
	# Elapsed time: 3.025 min

	write_csv(hurr.all.obs.full, "data/hurdat2-oisst-1981-2016.csv")
}

# hurr.all.obs <- fread("data/hurdat2-1981-2016.csv")
# hurr.all.obs.full <- fread("data/hurdat2-oisst-1981-2016.csv")

# write_csv(hurr.all.obs.full %>% filter(storm.id == "AL122005"), "data/katrina-oisst-clean.csv")
