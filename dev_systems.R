# Code to analyse of time stability of non-developing systems
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

library(tidyverse)

# Get RAW data ---------------------------------------------

pdi.all <- as_tibble(data.table::fread('data/hurdat2-hadisst-1966-2016_pdis.csv')) %>%
# pdi.all <- as_tibble(data.table::fread('data/hurdat2-hadisst-1966-2016_pdis_with_month.csv')) %>%
	mutate(storm.duration = measurements::conv_unit(storm.duration, "sec", "hr"))

pdi.natl <- pdi.all %>%
	dplyr::filter(basin == "NATL")
pdi.epac <- pdi.all %>%
	dplyr::filter(basin == "EPAC")

construct_data <- function(pdi.df) {
	df <- pdi.df %>%
		mutate(dev = ifelse(max.wind <= 33, "NDS", "DS")) %>%
		# group_by(storm.year, storm.month, basin, sst.class) %>%
		group_by(storm.year,basin, sst.class) %>%
		summarise(
			ds = sum(dev == "DS"),
			nds = sum(dev == "NDS"),
			total = ds + nds
		)
	return(df)
}

plot_dev_systems_ratio <- function(pdi.df, type = "ratio") {
	df <- construct_data(pdi.df)

	gg <- ggplot(df) +
		aes(x = storm.year)

	if (type == "ratio") {
		gg <- gg +
			geom_line(aes(y = ratio)) +
			geom_point(aes(y = ratio, colour = interaction(basin, sst.class))) +
			geom_hline( aes(yintercept = mean(ratio)), linetype = "dashed") +
			scale_y_continuous(limits = c(0, 1)) +
			labs(title = "Ratio of tropical depressions (v ≤ 33) per year",
					 colour = "SST Class")
	} else if (type == "total") {
		gg <- gg +
			geom_line(aes(y = nds)) +
			geom_point(aes(y = nds, colour = interaction(basin, sst.class))) +
			geom_hline( aes(yintercept = mean(nds)), linetype = "dashed") +
			scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,10)) +
			labs(title = "Number of tropical depressions (v ≤ 33) per year",
					 colour = "SST Class")
	} else if (type == "total-ds") {
		gg <- gg +
			geom_line(aes(y = ds) ) +
			geom_point(aes(y = ds, colour = interaction(basin, sst.class))) +
			geom_hline( aes(yintercept = mean(ds)), linetype = "dashed") +
			# scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,10)) +
			labs(title = "Number of tropical-cyclones (v > 33) per year",
					 colour = "SST Class")
	}


	gg <- gg +
		scale_colour_manual(values = c("NATL.high" = "brown1", "NATL.low" = "dodgerblue1",
																	 "EPAC.high" = "brown1", "EPAC.low" = "dodgerblue1")) +
		# scale_color_manual(values = c("EPAC" = "mediumseagreen", "NATL" = "mediumslateblue")) +
		theme_bw()

	return(gg)
}

# plot_dev_systems_ratio(pdi.natl, "total") +
# 	facet_wrap(~ sst.class, nrow = 2) +
# 	guides(colour = FALSE) +
# 	ggsave(filename = "ds-natl-total.pdf", width = 4, height = 4, dpi = 96, device = cairo_pdf)
#
# plot_dev_systems_ratio(pdi.natl) + facet_wrap(~ sst.class, nrow = 2) +
# 	facet_wrap(~ sst.class, nrow = 2) +
# 	guides(colour = FALSE) +
# 	ggsave(filename = "ds-natl-ratio.pdf", width = 4, height = 4, dpi = 96, device = cairo_pdf)
#
#
# plot_dev_systems_ratio(pdi.epac, "total") + facet_wrap(~ sst.class, nrow = 2) +
# 	facet_wrap(~ sst.class, nrow = 2) +
# 	guides(colour = FALSE) +
# 	ggsave(filename = "ds-epac-total.pdf", width = 4, height = 4, dpi = 96, device = cairo_pdf)
# plot_dev_systems_ratio(pdi.epac) + facet_wrap(~ sst.class, nrow = 2) +
# 	facet_wrap(~ sst.class, nrow = 2) +
# 	guides(colour = FALSE) +
# 	ggsave(filename = "ds-epac-ratio.pdf", width = 4, height = 4, dpi = 96, device = cairo_pdf)
#
# plot_dev_systems_ratio(pdi.all, "total") +
# 	facet_grid(~ basin) +
# 	ggsave(filename = "ds-all-total.pdf", width = 6, height = 3, dpi = 96, device = cairo_pdf)
#
# plot_dev_systems_ratio(pdi.all) +
# 	facet_grid(~ basin) +
# 	ggsave(filename = "ds-all-ratio.pdf", width = 6, height = 3, dpi = 96, device = cairo_pdf)
#
# plot_dev_systems_ratio(pdi.all, "total-ds") +
# 	facet_grid(~ basin) +
# 	ggsave(filename = "ds-all-total-ds.pdf", width = 6, height = 3, dpi = 96, device = cairo_pdf)



# Test -----------------------------------------------------

# library(TTR)
a <- construct_data(pdi.natl)%>%
	# select(storm.year,  sst.class) %>%
	# mutate(Dates = lubridate::as_date(paste(storm.year, 1, 1, sep = "-"), "%y-%m-%d"))
	# mutate(Dates = lubridate::as_date(paste(storm.year, storm.month, 1, sep = "-"), "%y-%m-%d"))
	mutate(Dates = as.Date(as.character(paste(storm.year, 01, 01, sep = "-"), "%Y-%m-%d")))

a.low <- a %>%
	filter(sst.class == "low")

a.high <- a %>%
	filter(sst.class == "high")

# a.dates <- tibble(Dates = seq(as.Date("1966/1/1"), as.Date("2016/12/1"), by = "year"))
#
# a.low <- full_join(a.low, a.dates)
# a.low <- arrange(a.low, Dates) #%>%
	#mutate(ds = ifelse(is.na(ds), 0, ds))

# library(fractal)
# library(locits)
library(aTSA)

# If your data is sufficient, studies are efficient , methods are sensitive and you are unbaised. These four provides completeness.

kpss.test(a.low$ds) # S
kpss.test(a.low$nds) # NS
kpss.test(a.low$total) # S

kpss.test(a.high$ds) # S
kpss.test(a.high$nds) # NS
kpss.test(a.high$total) # S

kpss.test(a$ds) # S
kpss.test(a$nds) # NS
kpss.test(a$total) # S


ggplot(a.low) +
	aes(x = Dates) +
	geom_line(aes(y = ds, colour = "big")) +
	geom_line(aes(y = nds, colour = "small")) +
	geom_line(aes(y = ds + nds, colour = "total"))

ggplot(a) +
	aes(x = Dates) +
	geom_line(aes(y = ds, colour = "big")) +
	geom_line(aes(y = nds, colour = "small")) +
	geom_line(aes(y = ds + nds, colour = "total")) +
	facet_wrap(~sst.class)

ggplot(a.low) +
	aes(x = Dates) +
	geom_line(aes(y = ds, colour = "big", linetype = "low")) +
	geom_line(aes(y = nds, colour = "small", linetype = "low")) +
	geom_line(aes(y = ds + nds, colour = "total", linetype = "low")) +
	geom_line(data = a.high, aes(y = ds, colour = "big", linetype = "high")) +
	geom_line(data = a.high, aes(y = nds, colour = "small", linetype = "high")) +
	geom_line(data = a.high, aes(y = ds + nds, colour = "total", linetype = "high"))
