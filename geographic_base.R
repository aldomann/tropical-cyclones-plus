# Base code to analise the influence of the geographical track of on the duration of hurricanes
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Libraries ------------------------------------------------
library(tidyverse)
# library(lubridate)
library(data.table)

# Haversine formula ----------------------------------------

haversine_distance <- function(lat1, lat2, lon1, lon2) {
	earth.radius = 6371000

	lat1 = lat1 * (pi/180)
	lat2 = lat2 * (pi/180)
	lon1 = lon1 * (pi/180)
	lon2 = lon2 * (pi/180)
	delta.lat = lat2 - lat1
	delta.lon = lon2 - lon1

	a <- sin(delta.lat/2) * sin(delta.lat/2) + cos(lat1) * cos(lat2) * sin(delta.lon/2) * sin(delta.lon/2)
	c <- 2 * atan2(sqrt(a), sqrt(1-a))

	return(earth.radius * c)
}
