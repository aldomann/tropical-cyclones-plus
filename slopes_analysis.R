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
	dplyr::filter(basin == "EPAC")

# Scatterplots ---------------------------------------------

plot_scatterplot(pdi.natl) + theme_bw()
plot_scatterplot(pdi.epac) + theme_bw()

plot_scatterplot(pdi.natl %>% dplyr::filter(max.wind > 33)) + theme_bw()
plot_scatterplot(pdi.epac %>% dplyr::filter(max.wind > 33)) + theme_bw()

# Confidence interval --------------------------------------

ci.natl.low <- get_conf_interval(pdi.natl, "low")
ci.natl.high <- get_conf_interval(pdi.natl, "high")

ci.epac.low <- get_conf_interval(pdi.epac, "low")
ci.epac.high <- get_conf_interval(pdi.epac, "high")

ci.natl.low.ds <- get_conf_interval(pdi.natl %>% dplyr::filter(max.wind > 33), "low")
ci.natl.high.ds <- get_conf_interval(pdi.natl %>% dplyr::filter(max.wind > 33), "high")

ci.epac.low.ds <- get_conf_interval(pdi.epac %>% dplyr::filter(max.wind > 33), "low")
ci.epac.high.ds <- get_conf_interval(pdi.epac %>% dplyr::filter(max.wind > 33), "high")

# Permutation test -----------------------------------------

# All data
do_permutation_test(pdi.natl)
do_permutation_test(pdi.epac)

# Developing systems
do_permutation_test(pdi.natl %>% dplyr::filter(max.wind > 33))
do_permutation_test(pdi.epac %>% dplyr::filter(max.wind > 33))


