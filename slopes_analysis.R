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

# Scatterplots ---------------------------------------------

plot_scatterplot(pdi.natl) + labs(title = "NATL") + theme_bw()
plot_scatterplot(pdi.epac) + labs(title = "EPAC") + theme_bw()

plot_scatterplot(pdi.natl %>% dplyr::filter(max.wind > 33)) + labs(title = "NATL DS") + theme_bw()
plot_scatterplot(pdi.epac %>% dplyr::filter(max.wind > 33)) + labs(title = "EPAC DS")+ theme_bw()

# Confidence interval for y~x ------------------------------

# ci.yx.natl.low <- get_conf_interval(pdi.natl, "low")
# ci.yx.natl.high <- get_conf_interval(pdi.natl, "high")

# ci.yx.epac.low <- get_conf_interval(pdi.epac, "low")
# ci.yx.epac.high <- get_conf_interval(pdi.epac, "high")

ci.yx.natl.low.ds <- get_conf_interval(pdi.natl %>% dplyr::filter(max.wind > 33), "low")
ci.yx.natl.high.ds <- get_conf_interval(pdi.natl %>% dplyr::filter(max.wind > 33), "high")

ci.yx.epac.low.ds <- get_conf_interval(pdi.epac %>% dplyr::filter(max.wind > 33), "low")
ci.yx.epac.high.ds <- get_conf_interval(pdi.epac %>% dplyr::filter(max.wind > 33), "high")

# Confidence interval for x~y ------------------------------

# ci.xy.natl.low <- get_conf_interval(pdi.natl, "low", reg = "x~y")
# ci.xy.natl.high <- get_conf_interval(pdi.natl, "high", reg = "x~y")

# ci.xy.epac.low <- get_conf_interval(pdi.epac, "low", reg = "x~y")
# ci.xy.epac.high <- get_conf_interval(pdi.epac, "high", reg = "x~y")

ci.xy.natl.low.ds <- get_conf_interval(pdi.natl %>% dplyr::filter(max.wind > 33), "low", reg = "x~y")
ci.xy.natl.high.ds <- get_conf_interval(pdi.natl %>% dplyr::filter(max.wind > 33), "high", reg = "x~y")

ci.xy.epac.low.ds <- get_conf_interval(pdi.epac %>% dplyr::filter(max.wind > 33), "low", reg = "x~y")
ci.xy.epac.high.ds <- get_conf_interval(pdi.epac %>% dplyr::filter(max.wind > 33), "high", reg = "x~y")


# Permutation test for y~x ---------------------------------

# All data
p.val.yx.natl <- do_permutation_test(pdi.natl)
p.val.yx.epac <- do_permutation_test(pdi.epac)

# Developing systems
p.val.yx.natl.ds <- do_permutation_test(pdi.natl %>% dplyr::filter(max.wind > 33))
p.val.yx.epac.ds <- do_permutation_test(pdi.epac %>% dplyr::filter(max.wind > 33))


# Permutation test for x~y ---------------------------------

# All data
p.val.xy.natl <- do_permutation_test(pdi.natl, reg = "x~y")
p.val.xy.epac <- do_permutation_test(pdi.epac, reg = "x~y")

# Developing systems
p.val.xy.natl.ds <- do_permutation_test(pdi.natl %>% dplyr::filter(max.wind > 33), reg = "x~y")
p.val.xy.epac.ds <- do_permutation_test(pdi.epac %>% dplyr::filter(max.wind > 33), reg = "x~y")

