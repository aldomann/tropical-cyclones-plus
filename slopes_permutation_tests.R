# Code to perform different statistical tests to analise the slopes of PDI vs duration
# Author: Alfredo Hernández <aldomann.designs@gmail.com>


# Libraries ------------------------------------------------
library(measurements) # Convert units

# Source base code -----------------------------------------
source("slopes_base.R")
# load("slopes_analysis.RData")

# Get RAW data ---------------------------------------------

pdi.all <- as_tibble(data.table::fread('data/hurdat2-hadisst-1966-2016_pdis.csv')) %>%
	mutate(storm.duration = conv_unit(storm.duration, "sec", "hr"))

pdi.natl <- pdi.all %>%
	dplyr::filter(basin == "NATL")
pdi.epac <- pdi.all %>%
	dplyr::filter(basin == "EPAC")

compute.flag <- T

# Permutation test for all data ----------------------------

if (compute.flag) {
	# NATL
	p.vals.natl.pdi <- summarise_p_values("NATL", "storm.duration", "storm.pdi")
	p.vals.natl.max.wind <- summarise_p_values("NATL", "storm.duration", "max.wind")
	p.vals.natl.mean.wind <- summarise_p_values("NATL", "storm.duration", "mean.wind")
	p.vals.natl.mean.sq.wind <- summarise_p_values("NATL", "storm.duration", "mean.sq.wind")

	# EPAC
	p.vals.epac.pdi <- summarise_p_values("EPAC", "storm.duration", "storm.pdi")
	p.vals.epac.max.wind <- summarise_p_values("EPAC", "storm.duration", "max.wind")
	p.vals.epac.mean.wind <- summarise_p_values("EPAC", "storm.duration", "mean.wind")
	p.vals.epac.mean.sq.wind <- summarise_p_values("EPAC", "storm.duration", "mean.sq.wind")
}


# Permutation test for developing systems ------------------

if (compute.flag) {
	# NATL
	p.vals.natl.pdi.ds <- summarise_p_values("NATL", "storm.duration", "storm.pdi", 33)
	p.vals.natl.max.wind.ds <- summarise_p_values("NATL", "storm.duration", "max.wind", 33)
	p.vals.natl.mean.wind.ds <- summarise_p_values("NATL", "storm.duration", "mean.wind", 33)
	p.vals.natl.mean.sq.wind.ds <- summarise_p_values("NATL", "storm.duration", "mean.sq.wind", 33)

	# EPAC
	p.vals.epac.pdi.ds <- summarise_p_values("EPAC", "storm.duration", "storm.pdi", 33)
	p.vals.epac.max.wind.ds <- summarise_p_values("EPAC", "storm.duration", "max.wind", 33)
	p.vals.epac.mean.wind.ds <- summarise_p_values("EPAC", "storm.duration", "mean.wind", 33)
	p.vals.epac.mean.sq.wind.ds <- summarise_p_values("EPAC", "storm.duration", "mean.sq.wind", 33)
}

# Tidy p-values in a list ----------------------------------

if (compute.flag) {
	# Group data frames into a list
	rm(p.values.list)
	p.values.list <- lapply(ls(patt='^p.vals.'), get)
	# rm(list=ls(pattern="^p.vals."))
}

# Analyse p-values -----------------------------------------

alpha = 0.05

	# Print regressions with p-value <= alpha
	for (i in 1:length(p.values.list)) {
		for (j in 1:2) {
			if ( (p.values.list[[i]][["slope.p.val"]][j] <= alpha) |
					 (p.values.list[[i]][["inter.p.val"]][j] <= alpha) |
					 (p.values.list[[i]][["total.p.val"]][j] <= alpha) ) {
				print(p.values.list[[i]][j,])
			}
		}
	}

# NATL (all storms)
p.values.list[lapply(purrr::map(p.values.list, ~dplyr::filter(.x, basin == "NATL", min.speed == 0)), nrow) > 0]

# NATL (all storms)
p.values.list[lapply(purrr::map(p.values.list, ~dplyr::filter(.x, basin == "EPAC", min.speed == 0)), nrow) > 0]

# NATL (developing systems)
p.values.list[lapply(purrr::map(p.values.list, ~dplyr::filter(.x, basin == "NATL", min.speed == 33)), nrow) > 0]

# EPAC (developing systems)
p.values.list[lapply(purrr::map(p.values.list, ~dplyr::filter(.x, basin == "EPAC", min.speed == 33)), nrow) > 0]
