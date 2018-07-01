# Base code to analise the influence of the geographical track of on the duration of hurricanes
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Libraries ------------------------------------------------
library(tidyverse)
# library(lubridate)
library(data.table)
library(maps)
library(ggalt)

# Distance formulas ----------------------------------------

# Haversine Formula
distance_haversine <- function(lat1, lat2, lon1, lon2) {
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

# Spherical Law of Cosines
distance_slc <- function(lat1, lat2, lon1, lon2) {
	earth.radius = 6371000

	lat1 = lat1 * (pi/180)
	lat2 = lat2 * (pi/180)
	lon1 = lon1 * (pi/180)
	lon2 = lon2 * (pi/180)
	delta.lon = lon2 - lon1

	distance <- acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(delta.lon)) * earth.radius

	return(as.numeric(distance))
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
		scale_size_continuous(range = c(0.2,1)) +
		labs(colour = "SST class")

	if (facet == T) {
		gg <- gg +
			facet_wrap(~sst.class)
	}

	return(gg)
}

map_region_hurrs_full <- function(storms.obs, coords, rect.coords.a, rect.coords.b, steps = c(5,5), xtra.lims = c(1.5,1.5), facet = F){
	# Populate SST Class into storms data frame
	storms.obs <- plyr::join(storms.obs,
													 storms.joint %>% select(storm.id, sst.class, distance))

	# Coordinates
	coords <- morph_coords(coords)
	rect.coords.a <- morph_coords(rect.coords.a)
	rect.coords.b <- morph_coords(rect.coords.b)

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
		annotate("rect", xmin = as.integer(rect.coords.a[1]), xmax = as.integer(rect.coords.a[2]),
						 ymin = as.integer(rect.coords.a[3]), ymax = as.integer(rect.coords.a[4]),
						 colour = "green", alpha = 0.2) +
		annotate("rect", xmin = as.integer(rect.coords.b[1]), xmax = as.integer(rect.coords.b[2]),
						 ymin = as.integer(rect.coords.b[3]), ymax = as.integer(rect.coords.b[4]),
						 colour = "green", alpha = 0.2) +
		scale_colour_manual(values = c("high" = "brown1", "low" = "dodgerblue1")) +
		scale_size_continuous(range = c(0.2,1)) +
		labs(colour = "SST class")

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
							 aes(x = storm.duration, y = distance/1000, colour = "low"),
							 shape = 5, size = 1) +
		# geom_smooth(data = df %>% dplyr::filter(sst.class == "low"),
		# 					 aes(x = storm.duration, y = distance, colour = "low"),
		# 					 method = "lm") +
		# High SST Years
		geom_point(data = df %>% dplyr::filter(sst.class == "high"),
							 aes(x = storm.duration, y = distance/1000, colour = "high"),
							 shape = 1, size = 1) +
		# geom_smooth(data = df %>% dplyr::filter(sst.class == "high"),
		# 						aes(x = storm.duration, y = distance, colour = "high"),
		# 						method = "lm") +
		scale_x_log10(breaks = c(25, 50, 100, 200, 400, 800)) +
		# scale_y_log10(breaks = c(125, 250, 500, 1000, 2000, 4000, 8000, 16000)) +
		scale_y_log10(breaks = c(125, 500, 2000, 8000)) +
		scale_color_manual(values = c("high" = "brown1", "low" = "dodgerblue1")) +
		labs(colour = "SST class",
				 x = "Storm lifetime (h)", y = "Path length (km)")
	return(gg)
}

# Plot histogram of distance/duration
plot_distance_histogram <- function(basin.name, min.speed = 0, facet = F) {
	df <- storms.joint %>%
		dplyr::filter(max.wind > min.speed) %>%
		dplyr::filter(basin == basin.name)

	gg <- ggplot(df) +
		geom_histogram(aes(fill = sst.class), colour = "black", alpha = 0.5, bins = 50, position) +
		aes(x = storm.duration/distance) +
		scale_color_manual(values = c("high" = "brown1", "low" = "dodgerblue1")) +
		labs(fill = "SST class")

	if (facet == T) {
		gg <- gg +
			facet_wrap(~sst.class)
	}

	return(gg)
}

plot_distance_histogram_alt <- function(basin.name, min.speed) {
	data <- storms.joint %>%
		dplyr::filter(max.wind > min.speed) %>%
		dplyr::filter(basin == basin.name) %>%
		dplyr::mutate(durperdist = distance/(storm.duration * 1000) )

	mean.low <- data %>%
		dplyr::filter(sst.class == "low") %>%
		select(durperdist) %>%
		unlist() %>%
		# log10() %>%
		mean()
	mean.high <- data %>%
		dplyr::filter(sst.class == "high") %>%
		select(durperdist) %>%
		unlist() %>%
		# log10() %>%
		mean()

	gg <- ggplot(data) +
		aes(x = durperdist, fill = sst.class) +
		# stat_density(geom="line", position = "identity")+
		geom_histogram(position = "identity", colour = "black", alpha = 0.5, bins = 50)+
		geom_vline(xintercept = mean.low,
							 colour = "dodgerblue1", linetype = "dashed") +
		geom_vline(xintercept = mean.high,
							 colour = "brown1", linetype = "dashed") +
		scale_fill_manual(values = c("high" = "brown1", "low" = "dodgerblue1")) +
		labs(x = "Mean forward speed (km/h)", y = "Count", fill = "SST Class") +
		theme_bw()

	return(gg)
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

plot_position_densities <- function(basin.name, variable, x.name, min.speed = 33) {
	data.epac <- storms.joint %>%
		dplyr::filter(basin == "EPAC") %>%
		dplyr::filter(
			first.long < 0,
			last.long < 0
		)

	data.natl <- storms.joint %>%
		dplyr::filter(basin == "NATL")

	data <- rbind(data.epac, data.natl) %>%
		dplyr::filter(max.wind > min.speed) %>%
		dplyr::filter(basin == basin.name)

	mean.low <- data %>%
		dplyr::filter(sst.class == "low") %>%
		select(variable) %>%
		unlist() %>%
		mean()
	mean.high <- data %>%
		dplyr::filter(sst.class == "high") %>%
		select(variable) %>%
		unlist() %>%
		mean()

	gg <- ggplot(data) +
		aes_string(x = variable, colour = "sst.class") +
		stat_density(geom="line", position = "identity")+
		geom_vline(xintercept = mean.low,
							 colour = "dodgerblue1", linetype = "dashed") +
		geom_vline(xintercept = mean.high,
							 colour = "brown1", linetype = "dashed") +
		scale_colour_manual(values = c("high" = "brown1", "low" = "dodgerblue1")) +
		labs(x = paste(x.name, "(rad)"), y = "Density", colour = "SST Class") +
		theme_bw()

	return(gg)
}


# Statistical summary of positions
get_location_mean_summary <- function(basin.name, min.speed = 0) {
	std.error <- function(x) { sd(x)/sqrt(length(x)) }

	df <- storms.joint %>%
		dplyr::filter(max.wind > min.speed) %>%
		dplyr::filter(basin == basin.name) %>%
		group_by(sst.class) %>%
		dplyr::summarise(mean.first.lat = mean(first.lat),
										 sd.first.lat = std.error(first.lat),
										 mean.last.lat = mean(last.lat),
										 sd.last.lat = sd(last.lat)/sqrt(n()),
										 mean.first.long = mean(first.long),
										 sd.first.long = sd(first.long)/sqrt(n()),
										 mean.last.long = mean(last.long),
										 sd.last.long = sd(last.long)/sqrt(n()))

	return(df)
}

get_location_median_summary <- function(basin.name, min.speed = 0) {
	median.error <- function(x) { 1.253 * sd(x)/sqrt(length(x)) }

	df <- storms.joint %>%
		dplyr::filter(max.wind > min.speed) %>%
		dplyr::filter(basin == basin.name) %>%
		group_by(sst.class) %>%
		dplyr::summarise(median.first.lat = median(first.lat),
										 sd.first.lat = median.error(first.lat),
										 median.last.lat = median(last.lat),
										 sd.last.lat = median.error(last.lat),
										 median.first.long = median(first.long),
										 sd.first.long = median.error(first.long),
										 median.last.long = median(last.long),
										 sd.last.long = median.error(last.long))

	return(df)
}

# Unpaired Wilcoxon Test
perform_wilcox_test <- function(var, basin.name, min.speed = 0) {
	df <- storms.joint %>%
		dplyr::filter(max.wind > min.speed) %>%
		dplyr::filter(basin == basin.name)

	res <- wilcox.test(df[, var] ~ df[, "sst.class"],
										 exact = FALSE)

	return(res)
}


# Histogram of initial and final positions
plot_positions_boxplot <- function(basin.name, var, min.speed = 0) {

	# Filter data frame
	df <- storms.joint %>%
		dplyr::filter(max.wind > min.speed) %>%
		dplyr::filter(basin == basin.name)

	# Initialise ggplot object
	gg <- ggplot(df) +
		geom_boxplot(aes(x = sst.class, colour = sst.class), alpha = 1) +
		# scale_x_continuous(limits = c(-150,10)) +
		scale_color_manual(values = c("high" = "brown1", "low" = "dodgerblue1")) +
		labs(x = "SST Class", colour = "SST Class")

	# Specify variable
	gg.lat <- gg +
		aes_string(y = paste0(var, ".lat"))
	gg.long <- gg +
		aes_string(y = paste0(var, ".long"))

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


# Cluster analysis -----------------------------------------

plot_clusters <- function(basin.name, type, min.speed = 0, n.clust = 2) {
	storms.small <- storms.joint %>%
		dplyr::filter(basin == basin.name) %>%
		dplyr::filter(max.wind > min.speed)

	if (type == "first") {
		mat <- storms.small %>% select(sst.class, first.lat, first.long, distance)
	} else if (type == "last") {
		mat <- storms.small %>% select(sst.class, last.lat, last.long, distance)
	}

	# High SST
	mat.high <- mat %>%
		dplyr::filter(sst.class == "high") %>%
		select(-sst.class)
	clust.high <- hclust(dist(mat.high), method = "complete")
	tree.high <- cutree(clust.high, n.clust)

	# Low SST
	mat.low <- mat %>%
		dplyr::filter(sst.class == "low") %>%
		select(-sst.class)
	clust.low <- hclust(dist(mat.low), method = "complete")
	tree.low <- cutree(clust.low, n.clust)

	# Merge data with clustering results
	data.high <- as_tibble(cbind(mat.high, clust = as.factor(tree.high), sst.class = "high"))
	data.low <- as_tibble(cbind(mat.low, clust = as.factor(tree.low), sst.class = "low"))
	data.all <- rbind(data.high, data.low)

	# Plot
	gg <- ggplot(data.all) +
		aes(colour = clust) +
		scale_size_continuous(range = c(0.2, 3)) +
		facet_wrap( ~ sst.class)

	if (type == "first") {
		gg <- gg +
			aes(x = first.long, y = first.lat) +
			geom_point(aes(size = distance), shape = 1) +
			geom_encircle(s_shape=0.8)
	} else if (type == "last") {
		gg <- gg +
			aes(x = last.long, y = last.lat) +
			geom_point(aes(size = distance), shape = 1) +
			geom_encircle(s_shape=0.8)
	}

	return(gg)
}
