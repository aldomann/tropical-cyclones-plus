# Code to perform different statistical tests to analise the slopes of PDI vs duration
# Author: Alfredo Hernández <aldomann.designs@gmail.com>


# Libraries ------------------------------------------------
library(measurements) # Convert units

# Source base code -----------------------------------------
source("slopes_base.R")
load("slopes_analysis.RData")

# Get RAW data ---------------------------------------------

pdi.all <- fread('data/hurdat2-hadisst-1966-2016_pdis.csv') %>%
	mutate(storm.duration = conv_unit(storm.duration, "sec", "hr"))

pdi.natl <- pdi.all %>%
	dplyr::filter(basin == "NATL")
pdi.epac <- pdi.all %>%
	dplyr::filter(basin == "EPAC")


# Confidence interval for all storms -----------------------

# NATL
ci.natl.pdi <- summarise_conf_intervals("NATL", "storm.duration", "storm.pdi")
ci.natl.max.wind <- summarise_conf_intervals("NATL", "storm.duration", "max.wind")
ci.natl.mean.wind <- summarise_conf_intervals("NATL", "storm.duration", "mean.wind")
ci.natl.mean.sq.wind <- summarise_conf_intervals("NATL", "storm.duration", "mean.sq.wind")

# EPAC
ci.epac.pdi <- summarise_conf_intervals("EPAC", "storm.duration", "storm.pdi")
ci.epac.max.wind <- summarise_conf_intervals("EPAC", "storm.duration", "max.wind")
ci.epac.mean.wind <- summarise_conf_intervals("EPAC", "storm.duration", "mean.wind")
ci.epac.mean.sq.wind <- summarise_conf_intervals("EPAC", "storm.duration", "mean.sq.wind")


# Confidence interval for developing systems ---------------

# NATL
ci.natl.pdi.ds <- summarise_conf_intervals("NATL", "storm.duration", "storm.pdi", 33)
ci.natl.max.wind.ds <- summarise_conf_intervals("NATL", "storm.duration", "max.wind", 33)
ci.natl.mean.wind.ds <- summarise_conf_intervals("NATL", "storm.duration", "mean.wind", 33)
ci.natl.mean.sq.wind.ds <- summarise_conf_intervals("NATL", "storm.duration", "mean.sq.wind", 33)

# EPAC
ci.epac.pdi.ds <- summarise_conf_intervals("EPAC", "storm.duration", "storm.pdi", 33)
ci.epac.max.wind.ds <- summarise_conf_intervals("EPAC", "storm.duration", "max.wind", 33)
ci.epac.mean.wind.ds <- summarise_conf_intervals("EPAC", "storm.duration", "mean.wind", 33)
ci.epac.mean.sq.wind.ds <- summarise_conf_intervals("EPAC", "storm.duration", "mean.sq.wind", 33)


# Permutation test for all data ----------------------------

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


# Permutation test for developing systems ------------------

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


# Analyse p-values -----------------------------------------

p.values.list <- lapply(ls(patt='^p.vals'), get)

alpha = 0.05

# Print regressions with p-value <= alpha
for (i in 1:length(p.values.list)) {
	for (j in 1:2) {
		if (p.values.list[[i]][["p.value"]][1] <= alpha) {
			print(p.values.list[[i]][j,])
		}
	}
}


# Scatterplots ---------------------------------------------

plot_scatterplot(pdi.natl, "storm.duration", "storm.pdi") +
	labs(title = "NATL (all storms; 1966-2016)") + theme_bw()
plot_scatterplot(pdi.epac, "storm.duration", "storm.pdi") +
	labs(title = "EPAC (all storms; 1986-2016)") + theme_bw()

plot_scatterplot(pdi.natl, "storm.duration", "storm.pdi", 33) +
	labs(title = "NATL (developing systems; 1966-2016)") + theme_bw()
plot_scatterplot(pdi.epac, "storm.duration", "storm.pdi", 33) +
	labs(title = "EPAC (developing systems; 1986-2016)") + theme_bw()
