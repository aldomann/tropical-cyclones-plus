# Code to analise the influence of the geographical track of on the duration of hurricanes
# Author: Alfredo Hernández <aldomann.designs@gmail.com>


# Source base code -----------------------------------------
source("geographic_base.R")

# Get RAW data ---------------------------------------------

storms.all <- as_tibble(fread('data/hurdat2-all.csv'))

storms.natl <- storms.all %>%
	dplyr::filter(basin == "NATL") %>%
	dplyr::filter(storm.year >= 1966)

storms.epac <- storms.all %>%
	dplyr::filter(basin == "EPAC") %>%
	dplyr::filter(storm.year >= 1986)

storms.natl <- arrange(storms.natl, date.time)
storms.epac <- arrange(storms.epac, date.time)

storms.all <- rbind(storms.natl, storms.epac)

# write_csv(storms.natl, "data/hurdat2-natl.csv")
# write_csv(storms.epac, "data/hurdat2-epac.csv")


# Summarise geographical information -----------------------

# Summarise data frame

storms.tracks <- storms.all %>%
	group_by(storm.id) %>%
	mutate(distance = distance_slc(lat, lag(lat), long, lag(long))) %>%
	mutate(distance = ifelse(is.na(distance), 0, distance)) %>%
	summarise(first.lat = first(lat), last.lat = last(lat),
						first.long = first(long), last.long = last(long),
						distance = sum(distance))


# Read PDI data frame

pdi.all <- as_tibble(fread('data/hurdat2-hadisst-1966-2016_pdis.csv'))

# Join data frames by storm.id
storms.joint <- full_join(pdi.all, storms.tracks)

storms.joint <- storms.joint %>%
	dplyr::filter(!is.na(storm.name))

# Write CSV
# write_csv(storms.joint, 'data/hurdat2-hadisst-1966-2016_pdis_geo.csv')

data.epac <- storms.joint %>%
	dplyr::filter(basin == "EPAC") %>%
	dplyr::filter(
		first.long < 0,
		last.long < 0,
		first.lat < 25
	)

data.natl <- storms.joint %>%
	dplyr::filter(basin == "NATL")

storms.joint <- rbind(data.epac, data.natl)

storms.joint <- storms.joint %>%
	mutate(storm.duration = measurements::conv_unit(storm.duration, "sec", "hr"))


# Forward speed --------------------------------------------

# Histograms
plot_distance_histogram_alt("NATL", 33) + theme_bw() #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl-forward-speed.pdf", width = 6, height = 2.5, dpi = 96, device = cairo_pdf)
plot_distance_histogram_alt("EPAC", 33) + theme_bw() #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac-forward-speed.pdf", width = 6, height = 2.5, dpi = 96, device = cairo_pdf)

# Analysis of travelled distance ---------------------------

# Summary of the distances
storms.joint %>%
	group_by(sst.class, basin) %>%
	summarise(dist.mean = round(mean(distance)/1000))

# Summary of the durations
storms.joint %>%
	group_by(sst.class, basin) %>%
	summarise(dist.mean = round(mean(storm.duration)))

# Summarise storms with longest paths
get_longest_paths("NATL")
get_longest_paths("EPAC")

# Scatterplot of distance vs duration
plot_distance_scatterplot("NATL", 33) + theme_bw() #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl-distance-bvln.pdf", width = 5, height = 2.5, dpi = 96, device = cairo_pdf)
plot_distance_scatterplot("EPAC", 33) + theme_bw() #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac-distance-bvln.pdf", width = 5, height = 2.5, dpi = 96, device = cairo_pdf)


# Position marginals ---------------------------------------

# North Atlantic
plot_position_densities("NATL", "first.long", "Genesis longitude", 33) #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl-init-long.pdf", width = 4, height = 2.5, dpi = 96, device = cairo_pdf)

plot_position_densities("NATL", "last.long", "Death longitude", 33) #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl-final-long.pdf", width = 4, height = 2.5, dpi = 96, device = cairo_pdf)

plot_position_densities("NATL", "first.lat", "Genesis latitude", 33) #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl-init-lat.pdf", width = 4, height = 2.5, dpi = 96, device = cairo_pdf)

plot_position_densities("NATL", "last.lat", "Death longitude", 33) #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl-final-lat.pdf", width = 4, height = 2.5, dpi = 96, device = cairo_pdf)

# Northeast Pacific
plot_position_densities("EPAC", "first.long", "Genesis longitude", 33) #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac-init-long.pdf", width = 4, height = 2.5, dpi = 96, device = cairo_pdf)

plot_position_densities("EPAC", "last.long", "Death longitude", 33) #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac-final-long.pdf", width = 4, height = 2.5, dpi = 96, device = cairo_pdf)

plot_position_densities("EPAC", "first.lat", "Genesis latitude", 33) #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac-init-lat.pdf", width = 4, height = 2.5, dpi = 96, device = cairo_pdf)

plot_position_densities("EPAC", "last.lat", "Death latitude", 33) #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac-final-lat.pdf", width = 4, height = 2.5, dpi = 96, device = cairo_pdf)


storms.joint %>%
	group_by(sst.class, basin) %>%
	summarise(
		mean.first.long = round(mean(first.long), 2),
		sd.first.long = round(sd(first.long), 2),
		mean.first.lat = round(mean(first.lat), 2),
		sd.first.lat = round(sd(first.lat), 2),
		mean.last.long = round(mean(last.long), 2),
		sd.last.long = round(sd(last.long), 2),
		mean.last.lat = round(mean(last.lat), 2),
		sd.last.lat = round(sd(last.lat), 2)
	) %>%
	data.frame()

# Position clustering --------------------------------------

# Scatterplot of initial and final positions

plot_positions("NATL", "first", 33)
plot_positions("EPAC", "first", 33)

# Scatterplot of initial and final positions

plot_positions("NATL", "last", 33)
plot_positions("EPAC", "last", 33)

plot_clusters("NATL", "first", 33, n.clust = 2)
plot_clusters("NATL", "last", 33, n.clust = 2)

plot_clusters("EPAC", "first", 33, n.clust = 2)
plot_clusters("EPAC", "last", 33, n.clust = 2)
