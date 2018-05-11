# Code to analyse of time stability of non-developing systems
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

library(tidyverse)

# Get RAW data ---------------------------------------------

pdi.all <- as_tibble(data.table::fread('data/hurdat2-hadisst-1966-2016_pdis.csv')) %>%
	mutate(storm.duration = measurements::conv_unit(storm.duration, "sec", "hr"))

pdi.natl <- pdi.all %>%
	dplyr::filter(basin == "NATL")
pdi.epac <- pdi.all %>%
	dplyr::filter(basin == "EPAC")

construct_data <- function(pdi.df) {
	df <- pdi.df %>%
		mutate(dev = ifelse(max.wind <= 33, "NDS", "DS")) %>%
		group_by(storm.year, basin, sst.class) %>%
		summarise(
			ds = sum(dev == "DS"),
			nds = sum(dev == "NDS"),
			ratio = sum(dev == "NDS")/n()
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
			scale_y_continuous(limits = c(0, 1)) +
			labs(title = "Ratio of tropical depressions (v ≤ 33) per year",
					 colour = "SST Class")
	} else if (type == "total") {
		gg <- gg +
			geom_line(aes(y = nds)) +
			geom_point(aes(y = nds, colour = interaction(basin, sst.class))) +
			scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,10)) +
			labs(title = "Number of tropical depressions (v ≤ 33) per year",
					 colour = "SST Class")
	} else if (type == "total-ds") {
		gg <- gg +
			geom_line(aes(y = ds) ) +
			geom_point(aes(y = ds, colour = interaction(basin, sst.class))) +
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

plot_dev_systems_ratio(pdi.natl, "total") +
	facet_wrap(~ sst.class, nrow = 2) +
	guides(colour = FALSE) +
	ggsave(filename = "ds-natl-total.pdf", width = 4, height = 4, dpi = 96, device = cairo_pdf)

plot_dev_systems_ratio(pdi.natl) + facet_wrap(~ sst.class, nrow = 2) +
	facet_wrap(~ sst.class, nrow = 2) +
	guides(colour = FALSE) +
	ggsave(filename = "ds-natl-ratio.pdf", width = 4, height = 4, dpi = 96, device = cairo_pdf)


plot_dev_systems_ratio(pdi.epac, "total") + facet_wrap(~ sst.class, nrow = 2) +
	facet_wrap(~ sst.class, nrow = 2) +
	guides(colour = FALSE) +
	ggsave(filename = "ds-epac-total.pdf", width = 4, height = 4, dpi = 96, device = cairo_pdf)
plot_dev_systems_ratio(pdi.epac) + facet_wrap(~ sst.class, nrow = 2) +
	facet_wrap(~ sst.class, nrow = 2) +
	guides(colour = FALSE) +
	ggsave(filename = "ds-epac-ratio.pdf", width = 4, height = 4, dpi = 96, device = cairo_pdf)

plot_dev_systems_ratio(pdi.all, "total") +
	facet_grid(~ basin) +
	ggsave(filename = "ds-all-total.pdf", width = 6, height = 3, dpi = 96, device = cairo_pdf)

plot_dev_systems_ratio(pdi.all) +
	facet_grid(~ basin) +
	ggsave(filename = "ds-all-ratio.pdf", width = 6, height = 3, dpi = 96, device = cairo_pdf)

plot_dev_systems_ratio(pdi.all, "total-ds") +
	facet_grid(~ basin) +
	ggsave(filename = "ds-all-total-ds.pdf", width = 6, height = 3, dpi = 96, device = cairo_pdf)
