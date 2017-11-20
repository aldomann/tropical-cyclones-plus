# Code to populate the HURDAT2 data with SST data from OISST
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

library(tidyverse)
library(lubridate)
library(raster)
library(ncdf4)

hurr.all.obs <- data.table::fread("data/hurdat2-all.csv")

# Source OISST Function ------------------------------------

source("NOAA_OISST_ncdf4.R")

mask <- "data/lsmask.oisst.v2.nc"

# Mine SST data from OISST ---------------------------------

hurr.all.obs.new <- hurr.all.obs %>%
	mutate(date.time = ymd_hms(date.time)) %>%
	dplyr::filter(date.time >= "1981-09-01")

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

hurr.all.obs.small <- head(hurr.all.obs.new)

hurr.all.obs.small <- hurr.all.obs.small %>%
	rowwise() %>%
	mutate(sst = get_oisst_by_date(date.time, ifelse(long < 0, 360+long , long), lat))
