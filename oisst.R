library(raster)
library(ncdf4)

oisst.file <- "data2/avhrr-only-v2.19810913.nc.nc"
mask <- "data2/lsmask.oisst.v2.nc"

# oisst.brick <- brick(oisst.file)
# oisst.raster <- raster(oisst.file)

source('OISST_function.R')

# test <- extractOISST1day(oisst.file, mask, 2.2, 2.2, 0.225, 0.225)

extractOISST1day_bylocation <- function(long, lat){
	sst <- extractOISST1day(oisst.file, mask, long, long, lat, lat)
	return(sst)
}

sst <- extractOISST1day_bylocation(2.2,0.225)
sst
