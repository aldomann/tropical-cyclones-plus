# Code to perform different statistical tests to analise the slopes of PDI vs duration
# Author: Alfredo Hernández <aldomann.designs@gmail.com>


# Libraries ------------------------------------------------
library(measurements) # Convert units

# Source base code -----------------------------------------
source("slopes_base.R")

# Get RAW data ---------------------------------------------

pdi.all <- fread('data/hurdat2-hadisst-1966-2016.csv') %>%
	mutate(storm.duration = conv_unit(storm.duration, "sec", "hr"))

pdi.natl <- pdi.all %>%
	dplyr::filter(basin == "NATL")
pdi.epac <- pdi.all %>%
	dplyr::filter(basin == "EPAC") %>%
	dplyr::filter(storm.year >= 1986)


# Confidence interval for all storms -----------------------

# PDI ~ duration regression
ci.yx.natl.low <- get_conf_interval(
	pdi.natl, "low", "storm.duration", "storm.pdi")
ci.yx.natl.high <- get_conf_interval(
	pdi.natl, "high", "storm.duration", "storm.pdi")

ci.yx.epac.low <- get_conf_interval(
	pdi.epac, "low", "storm.duration", "storm.pdi")
ci.yx.epac.high <- get_conf_interval(
	pdi.epac, "high", "storm.duration", "storm.pdi")

# Duration ~ PDI regression
ci.xy.natl.low <- get_conf_interval(
	pdi.natl, "low", "storm.pdi", "storm.duration")
ci.xy.natl.high <- get_conf_interval(
	pdi.natl, "high", "storm.pdi", "storm.duration")

ci.xy.epac.low <- get_conf_interval(
	pdi.epac, "low", "storm.pdi", "storm.duration")
ci.xy.epac.high <- get_conf_interval(
	pdi.epac, "high", "storm.pdi", "storm.duration")


# Confidence interval for developing systems ---------------

# PDI ~ duration regression
ci.yx.natl.low.ds <- get_conf_interval(
	pdi.natl, "low", "storm.duration", "storm.pdi", 33)
ci.yx.natl.high.ds <- get_conf_interval(
	pdi.natl, "high", "storm.duration", "storm.pdi", 33)

ci.yx.epac.low.ds <- get_conf_interval(
	pdi.epac, "low", "storm.duration", "storm.pdi", 33)
ci.yx.epac.high.ds <- get_conf_interval(
	pdi.epac, "high", "storm.duration", "storm.pdi", 33)

# Duration ~ PDI regression
ci.xy.natl.low.ds <- get_conf_interval(
	pdi.natl, "low", "storm.pdi", "storm.duration", 33)
ci.xy.natl.high.ds <- get_conf_interval(
	pdi.natl, "high", "storm.pdi", "storm.duration", 33)

ci.xy.epac.low.ds <- get_conf_interval(
	pdi.epac, "low", "storm.pdi", "storm.duration", 33)
ci.xy.epac.high.ds <- get_conf_interval(
	pdi.epac, "high", "storm.pdi", "storm.duration", 33)


# Permutation test for all data ----------------------------

# PDI ~ duration regression
p.val.yx.natl <- do_permutation_test(pdi.natl, "storm.pdi", "storm.duration")
p.val.yx.epac <- do_permutation_test(pdi.epac, "storm.pdi", "storm.duration")

# Duration ~ PDI regression
p.val.xy.natl <- do_permutation_test(pdi.natl, "storm.duration", "storm.pdi")
p.val.xy.epac <- do_permutation_test(pdi.epac, "storm.duration", "storm.pdi")


# Permutation test for developing systems ------------------

# PDI ~ duration regression
p.val.yx.natl.ds <- do_permutation_test(pdi.natl, "storm.pdi", "storm.duration", 33)
p.val.yx.epac.ds <- do_permutation_test(pdi.epac, "storm.pdi", "storm.duration", 33)

# Duration ~ PDI regression
p.val.xy.natl.ds <- do_permutation_test(pdi.natl, "storm.duration", "storm.pdi", 33)
p.val.xy.epac.ds <- do_permutation_test(pdi.epac, "storm.duration", "storm.pdi", 33)


# Scatterplots ---------------------------------------------

plot_scatterplot(pdi.natl, "storm.duration", "storm.pdi") +
	labs(title = "NATL (all storms; 1966-2016)") + theme_bw()
plot_scatterplot(pdi.epac, "storm.duration", "storm.pdi") +
	labs(title = "EPAC (all storms; 1986-2016)") + theme_bw()

plot_scatterplot(pdi.natl, "storm.duration", "storm.pdi", 33) +
	labs(title = "NATL (developing systems; 1966-2016)") + theme_bw()
plot_scatterplot(pdi.epac, "storm.duration", "storm.pdi", 33) +
	labs(title = "EPAC (developing systems; 1986-2016)") + theme_bw()

