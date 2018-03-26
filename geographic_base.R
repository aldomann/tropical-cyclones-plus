# Base code to analise the influence of the geographical track of on the duration of hurricanes
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Libraries ------------------------------------------------
library(tidyverse)
# library(lubridate)
library(data.table)
library(maps)
library(ggalt)

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


# Basins maps functions ------------------------------------

# Transform basin coordinates into numbers
morph_coords <- function(coords){
	coords[1] <- ifelse(str_extract(coords[1], "[A-Z]") == "W",
											- as.numeric(str_extract(coords[1], "[^A-Z]+")),
											as.numeric(str_extract(coords[1], "[^A-Z]+")) )
	coords[2] <- ifelse(str_extract(coords[2], "[A-Z]") == "W",
											- as.numeric(str_extract(coords[2], "[^A-Z]+")),
											as.numeric(str_extract(coords[2], "[^A-Z]+")) )
	coords[3] <- ifelse(str_extract(coords[3], "[A-Z]") == "S",
											- as.numeric(str_extract(coords[3], "[^A-Z]+")),
											as.numeric(str_extract(coords[3], "[^A-Z]+")) )
	coords[4] <- ifelse(str_extract(coords[4], "[A-Z]") == "S",
											- as.numeric(str_extract(coords[4], "[^A-Z]+")),
											as.numeric(str_extract(coords[4], "[^A-Z]+")) )
	return(coords)
}

# Map showing the hurricanes in specified window
# SRC: http://stackoverflow.com/questions/33302424/format-latitude-and-longitude-axis-labels-in-ggplot
scale_x_longitude <- function(xmin = -180, xmax = 180, step = 1, xtra.lim = 1.5, ...) {
	xbreaks <- seq(xmin,xmax,step)
	xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 0, parse(text=paste0(-x,"^o", "*W")),
																											 ifelse(x > 0, parse(text=paste0(x,"^o", "*E")), x))))
	return(scale_x_continuous("Longitude", breaks = xbreaks, labels = xlabels,
														expand = c(0, 0), limits = c(xmin-xtra.lim, xmax+xtra.lim), ...))
}

scale_y_latitude <- function(ymin = -90, ymax = 90, step = 0.5, xtra.lim = 1.5, ...) {
	ybreaks <- seq(ymin,ymax,step)
	ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(-x,"^o", "*S")),
																											 ifelse(x > 0, parse(text=paste0(x,"^o", "*N")), x))))
	return(scale_y_continuous("Latitude", breaks = ybreaks, labels = ylabels,
														expand = c(0, 0), limits = c(ymin-xtra.lim, ymax+xtra.lim), ...))
}

# Install legacy version of ggalt (see https://github.com/hrbrmstr/ggalt/issues/33)
# devtools::install_github("rplzzz/ggalt", ref = "ggp221")
map_region_hurrs <- function(storms.obs, coords, rect.coords, steps = c(5,5), xtra.lims = c(1.5,1.5), facet = F){
	# Populate SST Class into storms data frame
	storms.obs <- plyr::join(storms.obs,
													 storms.joint %>% select(storm.id, sst.class, distance))

	# Coordinates
	coords <- morph_coords(coords)
	rect.coords <- morph_coords(rect.coords)

	# World map
	world_map <- map_data("world")
	world_map <- subset(world_map, region!="Antarctica")

	# Create map
	gg <- ggplot(data = world_map, aes(x = long, y = lat, group = group)) +
		geom_cartogram(map = world_map, aes(map_id = region), colour = "white", fill = "grey50") +
		scale_x_longitude(xmin = as.numeric(coords[1]), xmax = as.numeric(coords[2]),
											step = steps[1], xtra.lim = xtra.lims[1]) +
		scale_y_latitude(ymin = as.numeric(coords[3]), ymax = as.numeric(coords[4]),
										 step = steps[2], xtra.lim = xtra.lims[2]) +
		coord_proj("+proj=merc") +
		geom_path(data = storms.obs %>% dplyr::filter(sst.class == "low"),
							aes(x = long, y = lat, group = storm.id, colour = "low"), #size = distance),
							alpha = 0.2, size = 0.2) +
		geom_path(data = storms.obs %>% dplyr::filter(sst.class == "high"),
							aes(x = long, y = lat, group = storm.id, colour = "high"), #size = distance),
							alpha = 0.2, size = 0.2) +
		annotate("rect", xmin = as.integer(rect.coords[1]), xmax = as.integer(rect.coords[2]),
						 ymin = as.integer(rect.coords[3]), ymax = as.integer(rect.coords[4]),
						 colour = "green", alpha = 0.2) +
		scale_colour_manual(values = c("high" = "brown1", "low" = "dodgerblue1")) +
		scale_size_continuous(range = c(0.2,1))

	if (facet == T) {
		gg <- gg +
			facet_wrap(~sst.class)
	}

	return(gg)
}


# Distance analysis ----------------------------------------

# Get summary of storms with longest path
get_longest_paths <- function(basin.name, head.n = 6) {
	gg <- storms.joint %>%
		dplyr::filter(basin == basin.name) %>%
		dplyr::arrange(desc(distance)) %>%
		select(storm.id, storm.name, storm.year, distance)%>%
		head(n = head.n)

	return(gg)
}

# Plot scatterplot of distance vs duration
plot_distance_scatterplot <- function(basin.name, min.speed = 0) {
	df <- storms.joint %>%
		dplyr::filter(max.wind > min.speed) %>%
		dplyr::filter(basin == basin.name)

	gg <- ggplot(df) +
		# Low SST Years
		geom_point(data = df %>% dplyr::filter(sst.class == "low"),
							 aes(x = storm.duration, y = distance, colour = "low"),
							 shape = 5, size = 1) +
		geom_smooth(data = df %>% dplyr::filter(sst.class == "low"),
							 aes(x = storm.duration, y = distance, colour = "low"),
							 method = "lm") +
		# High SST Years
		geom_point(data = df %>% dplyr::filter(sst.class == "high"),
							 aes(x = storm.duration, y = distance, colour = "high"),
							 shape = 1, size = 1) +
		geom_smooth(data = df %>% dplyr::filter(sst.class == "high"),
								aes(x = storm.duration, y = distance, colour = "high"),
								method = "lm") +
		scale_x_log10(breaks = c(25, 50, 100, 200, 400, 800)) +
		scale_y_log10() +
		scale_color_manual(values = c("high" = "brown1", "low" = "dodgerblue1")) +
		labs(colour = "SST class")
}

# Scatterplot of initial and final positions
plot_positions <- function(basin.name, type = "all", min.speed = 0) {

	# Filter data frame
	df <- storms.joint %>%
		dplyr::filter(max.wind > min.speed) %>%
		dplyr::filter(basin == basin.name)

	# Initialise ggplot object
	gg <- ggplot(df) +
		aes(colour = sst.class, shape = sst.class,
				size = distance, alpha = distance)

	if (type == "first") {
		gg <- gg +
			geom_point(aes(x = first.long, y = first.lat))
	} else if (type == "last") {
		gg <- gg +
			geom_point(aes(x = last.long, y = last.lat))
	} else if (type == "all") {
		gg <- gg +
			geom_point(aes(x = last.long, y = last.lat)) +
			geom_point(aes(x = first.long, y = first.lat))
	}

	gg <- gg +
		scale_color_manual(values = c("high" = "brown1", "low" = "dodgerblue1")) +
		scale_shape_manual(values = c("high" = 1, "low" = 1)) +
		scale_size_continuous(range = c(0.25, 3)) +
		scale_alpha_continuous(range = c(0.25, 1))

	return(gg)
}


# Histogram of initial and final positions
plot_positions_histogram <- function(basin.name, var, min.speed = 0) {

	# Filter data frame
	df <- storms.joint %>%
		dplyr::filter(max.wind > min.speed) %>%
		dplyr::filter(basin == basin.name)

	# Initialise ggplot object
	gg <- ggplot(df) +
		geom_density(aes(fill = sst.class), alpha = 0.2) +
		# scale_x_continuous(limits = c(-150,10)) +
		scale_fill_manual(values = c("high" = "brown1", "low" = "dodgerblue1")) +
		labs(fill = "SST Class")

	# Specify variable
	gg.lat <- gg +
		aes_string(x = paste0(var, ".lat"))
	gg.long <- gg +
		aes_string(x = paste0(var, ".long"))

	# Facet using multiplot and GGally
	pm <- ggmatrix(list("lat" = gg.lat + theme(legend.position="lat"),
											"long" = gg.long + theme(legend.position="long")),
								 nrow = 1, ncol = 2,
								 legend = c(1,1),
								 xAxisLabels = c("Latitude", "Longitude")) +
		theme( #strip.background = element_rect(fill = "white"),
			strip.placement = "outside")
	return(pm)
}
