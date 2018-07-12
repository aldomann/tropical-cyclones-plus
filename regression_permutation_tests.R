# Code to perform hypothesis testing using permutation tests to analise the slopes of PDI vs duration
# Author: Alfredo Hernández <aldomann.designs@gmail.com>


# Source base code -----------------------------------------
source("regression_base.R")
source("resampling_base.R")
# load("regression_analysis.RData")

# Get RAW data ---------------------------------------------

pdi.all <- as_tibble(data.table::fread('data/hurdat2-hadisst-1966-2016_pdis.csv')) %>%
	mutate(storm.duration = measurements::conv_unit(storm.duration, "sec", "hr"))

pdi.natl <- pdi.all %>%
	dplyr::filter(basin == "NATL")
pdi.epac <- pdi.all %>%
	dplyr::filter(basin == "EPAC")

compute.flag <- F
read.flag <- T
save.flag <- F
bs.flag <- T

# Load objects from disk -----------------------------------

if (read.flag) {
	# Standard
	p.values.list <- readRDS("objects/regression_p_values_all.rds")
	p.values.list.pdi <- readRDS("objects/regression_p_values_pdi.rds")
	p.values.list.max.wind <- readRDS("objects/regression_p_values_maxwind.rds")
	p.values.list.mean.wind <- readRDS("objects/regression_p_values_meanwind.rds")
	p.values.list.mean.sq.wind <- readRDS("objects/regression_p_values_meansqwind.rds")

	# With bootstrap
	boot.p.values.list <- readRDS("objects/regression_p_values_boot_all.rds")
	boot.p.values.list.pdi <- readRDS("objects/regression_p_values_boot_pdi.rds")
	boot.p.values.list.max.wind <- readRDS("objects/regression_p_values_boot_maxwind.rds")
	boot.p.values.list.mean.wind <- readRDS("objects/regression_p_values_boot_meanwind.rds")
	boot.p.values.list.mean.sq.wind <- readRDS("objects/regression_p_values_boot_meansqwind.rds")

	# Latest run for PDI
	final.p.values.list.pdi <- readRDS("objects/regression_p_values_pdi_final.rds")
}

# Permutation tests ----------------------------------------

n.sim.test <- 1000

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

if (compute.flag) {
	# Group data frames into a list
	p.values.list <- lapply(ls(patt='^p.vals.'), get)
	p.values.list.pdi <- lapply(ls(patt='^p.vals.*pdi*'), get)
	p.values.list.max.wind <- lapply(ls(patt='^p.vals.*max.wind*'), get)
	p.values.list.mean.wind <- lapply(ls(patt='^p.vals.*mean.wind*'), get)
	p.values.list.mean.sq.wind <- lapply(ls(patt='^p.vals.*mean.sq.wind*'), get)

	# rm(list=ls(pattern="^p.vals."))
}

# Save into RDS files
if (save.flag && !bs.flag) {
	saveRDS(p.values.list, "objects/regression_p_values_all.rds")
	saveRDS(p.values.list.pdi, "objects/regression_p_values_pdi.rds")
	saveRDS(p.values.list.max.wind, "objects/regression_p_values_maxwind.rds")
	saveRDS(p.values.list.mean.wind, "objects/regression_p_values_meanwind.rds")
	saveRDS(p.values.list.mean.sq.wind, "objects/regression_p_values_meansqwind.rds")
}

if (save.flag && bs.flag) {
	saveRDS(p.values.list, "objects/regression_p_values_boot_all.rds")
	saveRDS(p.values.list.pdi, "objects/regression_p_values_boot_pdi.rds")
	saveRDS(p.values.list.max.wind, "objects/regression_p_values_boot_maxwind.rds")
	saveRDS(p.values.list.mean.wind, "objects/regression_p_values_boot_meanwind.rds")
	saveRDS(p.values.list.mean.sq.wind, "objects/regression_p_values_boot_meansqwind.rds")
}

# Analyse p-values -----------------------------------------

# Print regressions with p-value <= alpha
explore_p_values(p.values.list, 0.05)
explore_p_values(boot.p.values.list, 0.05)

# NATL
p.vals.natl.pdi.ds.ols <- final.p.values.list.pdi[[3]]
p.vals.natl.pdi.ds.boot <- final.p.values.list.pdi[[4]]

p.vals.natl.pdi.ds.ols[,c(3,1,11,7,5,9)]
p.vals.natl.pdi.ds.boot[,c(3,1,11,7,5,9)]

# EPAC
p.vals.epac.pdi.ds.ols <- final.p.values.list.pdi[[1]]
p.vals.epac.pdi.ds.boot <- final.p.values.list.pdi[[2]]

p.vals.epac.pdi.ds.ols[,c(3,1,11,7,5,9)]
p.vals.epac.pdi.ds.boot[,c(3,1,11,7,5,9)]

# Compare statistics and methods ---------------------------

# Compare slope/intercept with alt calculation
(stats.std <- compare_perm_statistics(p.values.list))
(stats.boot <- compare_perm_statistics(boot.p.values.list))

stats.std[[1]]
stats.std[[2]]
stats.boot[[1]]
stats.boot[[2]]

# Compare calculation methods
compare_perm_methods(p.values.list, boot.p.values.list, 1.75, 0.6)
