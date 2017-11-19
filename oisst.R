library(raster)
library(ncdf4)

source("OISST_function.R")

url <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/access/avhrr-only/201501/avhrr-only-v2.20150114.nc"

download.file(url, destfile = "data/temp.nc")

oisst.file <- "data/temp.nc"
mask <- "data/lsmask.oisst.v2.nc"


extractOISST1day_bylocation <- function(oisst.file, long, lat){
	sst <- extractOISST1day(oisst.file, mask, long, long, lat, lat)
	return(sst)
}

sst <- extractOISST1day_bylocation(oisst.file, 2.2,0.225)
sst



