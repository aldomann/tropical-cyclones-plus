# Code to perform different statistical tests to analise the slopes of PDI vs duration
# Author: Alfredo Hernández <aldomann.designs@gmail.com>


# Source base code -----------------------------------------
source("slopes_base.R")
# load("slopes_analysis.RData")

# Get RAW data ---------------------------------------------

pdi.all <- as_tibble(data.table::fread('data/hurdat2-hadisst-1966-2016_pdis.csv')) %>%
	mutate(storm.duration = measurements::conv_unit(storm.duration, "sec", "hr"))

pdi.natl <- pdi.all %>%
	dplyr::filter(basin == "NATL")
pdi.epac <- pdi.all %>%
	dplyr::filter(basin == "EPAC")

compute.flag <- T
bs.flag <- T

# Load objects from disk -----------------------------------

if (!compute.flag) {
	# Standard
	# p.values.list <- readRDS("slopes_p_values.rds")
	p.values.list.pdi <- readRDS("slopes_p_values_pdi.rds")
	p.values.list.max.wind <- readRDS("slopes_p_values_maxwind.rds")
	p.values.list.mean.wind <- readRDS("slopes_p_values_meanwind.rds")
	p.values.list.mean.sq.wind <- readRDS("slopes_p_values_meansqwind.rds")

	# With bootstrap
	# boot.p.values.list <- readRDS("slopes_p_values_boot.rds")
	boot.p.values.list.pdi <- readRDS("slopes_p_values_boot_pdi.rds")
	boot.p.values.list.max.wind <- readRDS("slopes_p_values_boot_maxwind.rds")
	boot.p.values.list.mean.wind <- readRDS("slopes_p_values_boot_meanwind.rds")
	boot.p.values.list.mean.sq.wind <- readRDS("slopes_p_values_boot_meansqwind.rds")
}

# Permutation tests ----------------------------------------

n.sim.test <- 50

# Permutation test for all data
if (compute.flag) {
	# NATL
	p.vals.natl.pdi <- summarise_p_values("NATL", "storm.duration", "storm.pdi", 0, bs.flag, n.sim.test)
	p.vals.natl.max.wind <- summarise_p_values("NATL", "storm.duration", "max.wind", 0, bs.flag, n.sim.test)
	p.vals.natl.mean.wind <- summarise_p_values("NATL", "storm.duration", "mean.wind", 0, bs.flag, n.sim.test)
	p.vals.natl.mean.sq.wind <- summarise_p_values("NATL", "storm.duration", "mean.sq.wind", 0, bs.flag, n.sim.test)

	# EPAC
	p.vals.epac.pdi <- summarise_p_values("EPAC", "storm.duration", "storm.pdi", 0, bs.flag, n.sim.test)
	p.vals.epac.max.wind <- summarise_p_values("EPAC", "storm.duration", "max.wind", 0, bs.flag, n.sim.test)
	p.vals.epac.mean.wind <- summarise_p_values("EPAC", "storm.duration", "mean.wind", 0, bs.flag, n.sim.test)
	p.vals.epac.mean.sq.wind <- summarise_p_values("EPAC", "storm.duration", "mean.sq.wind", 0, bs.flag, n.sim.test)
}


# Permutation test for developing systems
if (compute.flag) {
	# NATL
	p.vals.natl.pdi.ds <- summarise_p_values("NATL", "storm.duration", "storm.pdi", 33, bs.flag, n.sim.test)
	p.vals.natl.max.wind.ds <- summarise_p_values("NATL", "storm.duration", "max.wind", 33, bs.flag, n.sim.test)
	p.vals.natl.mean.wind.ds <- summarise_p_values("NATL", "storm.duration", "mean.wind", 33, bs.flag, n.sim.test)
	p.vals.natl.mean.sq.wind.ds <- summarise_p_values("NATL", "storm.duration", "mean.sq.wind", 33, bs.flag, n.sim.test)

	# EPAC
	p.vals.epac.pdi.ds <- summarise_p_values("EPAC", "storm.duration", "storm.pdi", 33, bs.flag, n.sim.test)
	p.vals.epac.max.wind.ds <- summarise_p_values("EPAC", "storm.duration", "max.wind", 33, bs.flag, n.sim.test)
	p.vals.epac.mean.wind.ds <- summarise_p_values("EPAC", "storm.duration", "mean.wind", 33, bs.flag, n.sim.test)
	p.vals.epac.mean.sq.wind.ds <- summarise_p_values("EPAC", "storm.duration", "mean.sq.wind", 33, bs.flag, n.sim.test)
}

# Tidy p-values in a list ----------------------------------

if (compute.flag && !bs.flag) {
	# Group data frames into a list
	rm(p.values.list)
	p.values.list <- lapply(ls(patt='^p.vals.'), get)
	p.values.list.pdi <- lapply(ls(patt='^p.vals.*pdi*'), get)
	p.values.list.max.wind <- lapply(ls(patt='^p.vals.*max.wind*'), get)
	p.values.list.mean.wind <- lapply(ls(patt='^p.vals.*mean.wind*'), get)
	p.values.list.mean.sq.wind <- lapply(ls(patt='^p.vals.*mean.sq.wind*'), get)

	# rm(list=ls(pattern="^p.vals."))

	saveRDS(p.values.list, "slopes_p_values.rds")
	saveRDS(p.values.list.pdi, "slopes_p_values_pdi.rds")
	saveRDS(p.values.list.max.wind, "slopes_p_values_maxwind.rds")
	saveRDS(p.values.list.mean.wind, "slopes_p_values_meanwind.rds")
	saveRDS(p.values.list.mean.sq.wind, "slopes_p_values_meansqwind.rds")
}

if (compute.flag && bs.flag) {
	# Group data frames into a list
	rm(boot.p.values.list)

	boot.p.values.list <- lapply(ls(patt='^boot.p.vals.'), get)
	boot.p.values.list.pdi <- lapply(ls(patt='^boot.p.vals.*pdi*'), get)
	boot.p.values.list.max.wind <- lapply(ls(patt='^boot.p.vals.*max.wind*'), get)
	boot.p.values.list.mean.wind <- lapply(ls(patt='^boot.p.vals.*mean.wind*'), get)
	boot.p.values.list.mean.sq.wind <- lapply(ls(patt='^boot.p.vals.*mean.sq.wind*'), get)

	# rm(list=ls(pattern="^boot.p.vals."))

	saveRDS(boot.p.values.list, "slopes_p_values_boot.rds")
	saveRDS(boot.p.values.list.pdi, "slopes_p_values_boot_pdi.rds")
	saveRDS(boot.p.values.list.max.wind, "slopes_p_values_boot_maxwind.rds")
	saveRDS(boot.p.values.list.mean.wind, "slopes_p_values_boot_meanwind.rds")
	saveRDS(boot.p.values.list.mean.sq.wind, "slopes_p_values_boot_meansqwind.rds")
}

# Analyse p-values -----------------------------------------

# Print regressions with p-value <= alpha
# explore_p_values(p.values.list, 0.05)
explore_p_values(p.values.list.pdi, 0.05)
explore_p_values(p.values.list.max.wind, 0.05)

# explore_p_values(boot.p.values.list, 0.05)
explore_p_values(boot.p.values.list.pdi, 0.05)
explore_p_values(boot.p.values.list.max.wind, 0.05)

# NATL (all storms)
# p.values.list[lapply(purrr::map(p.values.list, ~dplyr::filter(.x, basin == "NATL", min.speed == 0)), nrow) > 0][[4]]
#
# # NATL (all storms)
# p.values.list[lapply(purrr::map(p.values.list, ~dplyr::filter(.x, basin == "EPAC", min.speed == 0)), nrow) > 0][[4]]
#
# # NATL (developing systems)
# p.values.list[lapply(purrr::map(p.values.list, ~dplyr::filter(.x, basin == "NATL", min.speed == 33)), nrow) > 0][[4]]
#
# # EPAC (developing systems)
# p.values.list[lapply(purrr::map(p.values.list, ~dplyr::filter(.x, basin == "EPAC", min.speed == 33)), nrow) > 0][[4]]


# Compare raw permutation & bootstrap-powered permutation
for (i in 1:4) {
	print(boot.p.values.list.pdi[[i]])
	print(p.values.list.pdi[[i]])
	print("=======================================================================")
}

for (i in 1:4) {
	print(boot.p.values.list.max.wind[[i]])
	print(p.values.list.max.wind[[i]])
	print("=======================================================================")
}


# Compare slope/intercept with alt calculation -------------

slope.factor <- numeric(16)
inter.factor <- numeric(16)
for (i in 1:length(p.values.list)) {
	for (j in 1:2) {
		slope.factor[i] <- p.values.list[[i]][j, 1] / p.values.list[[i]][j, 5]
		inter.factor[i] <- p.values.list[[i]][j, 3] / p.values.list[[i]][j, 7]
	}
}
mean(slope.factor)
sd(slope.factor)
mean(inter.factor)
sd(inter.factor)
