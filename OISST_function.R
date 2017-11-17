require(ncdf4)	# install.packages('ncdf') if you don't already have it.
require(fields) # install.packages('fields') if you don't already have it.
extractOISST = function(fname,lsmask,lon1,lon2,lat1,lat2, date1, date2){

	# Generate set of grid cell latitudes (center of cell) from south to north
	lats = seq(-89.875,89.875,0.25)
	# Generate set of grid cell longitudes (center of cell)
	lons = seq(0.125,359.875,0.25)
	# Create connection to NetCDF data file
	nc = nc_open(fname)

	lon1indx = which.min(abs(lon1 - lons)) #get index of nearest longitude value
	if (missing(lon2)){
		# If lon2 isn't specified, reused lon1
		lon2 = lon1
		lon2indx = lon1indx
		cat("Only 1 longitude specified\n")
	} else {
		# Get index of nearest longitude value to lon2
		lon2indx = which.min(abs(lon2 - lons))
	}
	lat1indx = which.min(abs(lat1 - lats)) #get index of nearest latitude value
	if (missing(lat2)){
		# If lat2 is not specified, reuse lat1
		lat2 = lat1
		lat2indx = lat1indx
		cat("Only 1 latitude specified\n")
	} else {
		# Get index of nearest latitude value to lat2
		lat2indx = which.min(abs(lat2 - lats))
	}

	# The lon/lat indx values should now correspond to indices in the NetCDF
	# file for the desired grid cell.
	nlon = (lon2indx - lon1indx) + 1 # get number of longitudes to extract
	nlat = (lat2indx - lat1indx) + 1 # get number of latitudes to extract

	# Extract available dates from netCDF file
	ncdates = nc$dim$time$vals
	ncdates = as.Date(ncdates,origin = '1800-1-1') #available time points in nc

	if (class(date1) == 'Date'){
		# Get index of nearest time point
		date1indx = which.min(abs(date1 - ncdates))
	} else if (class(date1) == 'character'){
		# Convert to a Date object first
		date1 = as.Date(date1)
		date1indx = which.min(abs(date1 - ncdates))
	}
	if (missing(date2)) {
		# If date2 isn't specified, reuse date1
		date2indx = which.min(abs(date1 - ncdates))
		cat('Only 1 date specified\n')
	} else {
		if (class(date2) == 'Date'){
			# If date2 exists, get index of nearest time point to date2
			date2indx = which.min(abs(date2 - ncdates))
		} else if (class(date2) == 'character'){
			date2 = as.Date(date2)
			date2indx = which.min(abs(date2 - ncdates))
		}
	}

	ndates = (date2indx - date1indx) + 1 #get number of time steps to extract

	# Define the output array
	sstout = array(data = NA, dim = c(nlon,nlat,ndates))
	# Extract the data from the NetCDF file
	sstout[,,] = ncvar_get(nc, varid = 'sst',
			start = c(lon1indx,lat1indx,date1indx),
			count = c(nlon,nlat,ndates))

	# If there are missing data in the NetCDF, they should appear as 32767.
	# Replace that value with NA if it occurs anywhere.
	sstout = ifelse(sstout == 32767, NA, sstout)

	# Open the land-sea mask
	nc2 = nc_open(lsmask)
	# Create array to hold land-sea mask
	mask = array(data = NA, dim = c(nlon,nlat,1))
	# Get land-sea mask values (0 or 1)
	mask[,,] = ncvar_get(nc2, varid = "lsmask",
			start = c(lon1indx,lat1indx,1), count = c(nlon,nlat,1))
	# Replace 0's with NA's
	mask = ifelse(mask == 0,NA,1)

	# Get dimensions of sstout array
	dims = dim(sstout)
	for (i in 1:dims[3]){
		sstout[,,i] = sstout[,,i] * mask[,,1] # All masked values become NA
		# Add dimension names
		attr(sstout,'dimnames') = list(Long = seq(lons[lon1indx],lons[lon2indx],
						by = 0.25),
				Lat = seq(lats[lat1indx],lats[lat2indx],
						by = 0.25),
				Date = as.character(seq(ncdates[date1indx],
								ncdates[date2indx],by = 1)))
	}

	dims = dim(sstout) # get size of array

	sst2 = array(data = NA, dim = c(dims[2],dims[1],dims[3]),
			dimnames = list(Lat = rev(seq(lats[lat1indx],lats[lat2indx],
									by = 0.25)),
					Long = seq(lons[lon1indx],lons[lon2indx],by = 0.25),
					Date = as.character(seq(ncdates[date1indx],
									ncdates[date2indx],by = 1))))
	# Step through each page of array and rearrange lat/lon values
	for (i in 1:dims[3]){
		# Extract one day's worth of lat/lon pairs
		temp = as.matrix(sstout[,,i])
		temp = t(temp) # transpose lon/lat to lat/lon
		temp = temp[nrow(temp):1,] # reverse row order to reverse latitudes
		sst2[,,i] = temp # write data to sst2 array
	}

	##########################
	sst2 # return sst2 array
	##########################
} # end of function


extractOISST1day = function(fname,lsmask,lon1,lon2,lat1,lat2){
	# Generate set of grid cell latitudes (center of cell) from south to north
	lats = seq(-89.875,89.875,0.25)
	# Generate set of grid cell longitudes (center of cell)
	lons = seq(0.125,359.875,0.25)
	# Create connection to NetCDF data file (must be unzipped manually already)
	nc = nc_open(fname)

	lon1indx = which.min(abs(lon1 - lons)) #get index of nearest longitude value
	if (missing(lon2)){
		# If lon2 isn't specified, reused lon1
		lon2 = lon1
		lon2indx = lon1indx
		cat("Only 1 longitude specified\n")
	} else {
		# Get index of nearest longitude value to lon2
		lon2indx = which.min(abs(lon2 - lons))
	}
	lat1indx = which.min(abs(lat1 - lats)) #get index of nearest latitude value
	if (missing(lat2)){
		# If lat2 is not specified, reuse lat1
		lat2 = lat1
		lat2indx = lat1indx
		cat("Only 1 latitude specified\n")
	} else {
		# Get index of nearest latitude value to lat2
		lat2indx = which.min(abs(lat2 - lats))
	}

	# The lon/lat indx values should now correspond to indices in the NetCDF
	# file for the desired grid cell.
	nlon = (lon2indx - lon1indx) + 1 # get number of longitudes to extract
	nlat = (lat2indx - lat1indx) + 1 # get number of latitudes to extract
	# Extract the date from the file
	dateref = nc$dim$time$units
	dateref = sub('days since ','',dateref,ignore.case=TRUE)
	date1 = as.Date(nc$dim$time$vals[1],origin = dateref)
	# Although this extracts the date, I do not currently include it in the
	# output, as I assume you are already getting the date from the input
	# filename before running this function.

	# Define the output array
	sstout = matrix(data = NA, nrow = nlon, ncol = nlat)
	# single-day mean SST extraction
	sstout[,] = ncvar_get(nc, varid = 'sst',
			start = c(lon1indx,lat1indx,1,1),
			count = c(nlon,nlat,1,1))
	# Replace that value with NA if it occurs anywhere.
	sstout = ifelse(sstout == 32767, NA, sstout)

	# Open the land-sea mask
	nc2 = nc_open(lsmask)
	# Create array to hold land-sea mask
	mask = array(data = NA, dim = c(nlon,nlat,1))
	# Get land-sea mask values (0 or 1)
	mask[,,] = ncvar_get(nc2, varid = "lsmask",
			start = c(lon1indx,lat1indx,1), count = c(nlon,nlat,1))
	# Replace 0's with NA's
	mask = ifelse(mask == 0,NA,1)


	sstout[,] = sstout[,] * as.matrix(mask[,,1]) # All masked values become NA
		# Add dimension names
	attr(sstout,'dimnames') = list(Long = seq(lons[lon1indx],lons[lon2indx],
					by = 0.25), Lat = seq(lats[lat1indx],lats[lat2indx],
					by = 0.25))

	# south down the rows, and longitudes run from west to east across columns.
	sstout = t(sstout)
	sstout = sstout[nrow(sstout):1,]

	#############################
	sstout # return sstout matrix
	#############################
} # end of function


plotOISST = function(sst2, day = 1) {
	lats = as.numeric(dimnames(sst2)$Lat) # extract latitudes
	lons = as.numeric(dimnames(sst2)$Long) # extract longitudes
	par(mar = c(4.5,4.5,3,6)) # widen margins to fit colorbar
	image(x = lons, y = rev(lats),
			if (length(dim(sst2))>2){
						as.matrix(t(sst2[nrow(sst2):1, ,day]))
					} else {
						t(sst2[nrow(sst2):1,])
					},
			las = 1,
			ylim = range(lats),
			col = tim.colors(32),
			yaxt = "n", xaxt = "n",
			ylab = "Latitude (degrees north)",
			xlab = "Longitude (degrees east)",
			main = if(length(dim(sst2)>2)){
				dimnames(sst2)$Date[day] # extract date from array dimnames
			})
	axis(1,at = pretty(lons),
			labels = pretty(lons))
	axis(2,at = pretty(lats),
			labels = pretty(lats), las = 1)
	# image.plot from the fields package inserts a color bar

	image.plot(zlim = range(if(length(dim(sst2))>2){
						sst2[,,day]
					} else {
						sst2[,]
					}, na.rm=TRUE),
					nlevel = 32,
			legend.only = TRUE, horizontal = FALSE, col = tim.colors(32),
			legend.args = list(text = "Temperature, C", cex = 1.2,
					side = 4, line = 2))
}

