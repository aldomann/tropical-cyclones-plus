library(raster)
library(ncdf4)

hurr.all.obs <- data.table::fread("data/hurdat2-all.csv")

# OISST Function -------------------------------------------

source("OISST_function.R")

url <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/access/avhrr-only/201501/avhrr-only-v2.20150114.nc"
download.file(url, destfile = "data-tests/temp.nc")

oisst.file <- "data-tests/temp.nc"
mask <- "data/lsmask.oisst.v2.nc"

extractOISST1day_bylocation <- function(oisst.file, long, lat){
	sst <- extractOISST1day(oisst.file, mask, long, long, lat, lat)
	return(sst)
}

# Getting data ---------------------------------------------

sst <- extractOISST1day_bylocation(oisst.file, 2.2,0.225)
sst



