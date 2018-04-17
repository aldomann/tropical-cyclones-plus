# Base code to perform different statistical tests to analise the slopes of PDI vs duration
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Libraries ------------------------------------------------
library(tidyverse)
library(lubridate)

# Confidence interval --------------------------------------

summarise_marginals_stats <- function(basin.name, min.speed = 0) {
	std.error <- function(x) { sd(x)/sqrt(length(x)) }
	median.error <- function(x) { 1.253 * sd(x)/sqrt(length(x)) }

	data <- pdi.all %>%
		dplyr::filter(basin == basin.name) %>%
		dplyr::filter(max.wind > min.speed) %>%
		select(storm.pdi, storm.duration, max.wind, sst.class)

	data <- data %>%
		group_by(sst.class) %>%
		dplyr::summarise(
			pdi.mean = mean(storm.pdi)/1E-9,
			pdi.mean.err = std.error(storm.pdi)/1E-9,
			pdi.median = median(storm.pdi)/1E-9,
			pdi.median.err = median.error(storm.pdi)/1E-9,
			dur.mean = mean(storm.duration),
			dur.mean.err = std.error(storm.duration),
			dur.median = median(storm.duration),
			dur.median.err = median.error(storm.duration)
		)

	return(data)
}

# Analyse statistical compatibility ------------------------

analyse_marginals_compat <- function(basin.name, min.speed = 0) {

	data <- get_marginals_stats(basin.name, min.speed)

	is_overlapping <- function(x, err) {
		lower <- min(x) + err[[which.min(x)]]
		upper <- max(x) - err[[which.max(x)]]

		return(upper <= lower)
	}

	data <- data %>%
		summarise(
			pdi.mean.comp = is_overlapping(pdi.mean, pdi.mean.err),
			pdi.median.comp = is_overlapping(pdi.median, pdi.median.err),
			dur.mean.comp = is_overlapping(dur.mean, dur.mean.err),
			dur.median.comp = is_overlapping(dur.mean, dur.median.err)
		)

	return(data)
}
