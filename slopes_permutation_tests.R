# Code to perform different statistical tests to analise the slopes of PDI vs duration
# Author: Alfredo Hernández <aldomann.designs@gmail.com>


# Source base code -----------------------------------------
source("slopes_base.R")
# load("slopes_analysis.RData")

zug_zug <- function(call) {
	print(system.time(call))
	system("notify-send 'Finished calculations' 'Get back to work!' -i rstudio -u critical")
	system("paplay /usr/share/sounds/freedesktop/stereo/complete.oga")
}


# Get RAW data ---------------------------------------------

pdi.all <- as_tibble(data.table::fread('data/hurdat2-hadisst-1966-2016_pdis.csv')) %>%
	mutate(storm.duration = measurements::conv_unit(storm.duration, "sec", "hr"))

pdi.natl <- pdi.all %>%
	dplyr::filter(basin == "NATL")
pdi.epac <- pdi.all %>%
	dplyr::filter(basin == "EPAC")

compute.flag <- T
bootstrap.flag <- F

# Load objects from disk -----------------------------------

if (!compute.flag) {
	p.values.list <- readRDS("slopes_p_values.rds")
	boot.p.values.list <- readRDS("slopes_p_values_boot.rds")
}

# Permutation tests ----------------------------------------

# Permutation test for all data
if (compute.flag && !bootstrap.flag) {
	# Save initial time
	init.time <- Sys.time()

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


# Permutation test for developing systems
if (compute.flag && !bootstrap.flag) {
	# NATL
	p.vals.natl.pdi.ds <- summarise_p_values("NATL", "storm.duration", "storm.pdi", 33)
	p.vals.natl.pdi.ds.2 <- summarise_p_values("NATL", "storm.duration", "storm.pdi", 33)
	p.vals.natl.max.wind.ds <- summarise_p_values("NATL", "storm.duration", "max.wind", 33)
	p.vals.natl.mean.wind.ds <- summarise_p_values("NATL", "storm.duration", "mean.wind", 33)
	p.vals.natl.mean.sq.wind.ds <- summarise_p_values("NATL", "storm.duration", "mean.sq.wind", 33)

	# EPAC
	p.vals.epac.pdi.ds <- summarise_p_values("EPAC", "storm.duration", "storm.pdi", 33)
	p.vals.epac.max.wind.ds <- summarise_p_values("EPAC", "storm.duration", "max.wind", 33)
	p.vals.epac.mean.wind.ds <- summarise_p_values("EPAC", "storm.duration", "mean.wind", 33)
	p.vals.epac.mean.sq.wind.ds <- summarise_p_values("EPAC", "storm.duration", "mean.sq.wind", 33)

	# Save final time
	final.time <- Sys.time()
}

if (compute.flag && !bootstrap.flag) {
	elapsed.time <- final.time - init.time
	system("notify-send 'Finished calculations' 'Get back to work!' -i rstudio -u critical")
}

# Permutation tests with bootstrap -------------------------

# Permutation test for all data
if (compute.flag && bootstrap.flag) {
	# Save initial time
	init.time <- Sys.time()

	# NATL
	boot.p.vals.natl.pdi <- summarise_p_values("NATL", "storm.duration", "storm.pdi", bootstrap = T, n.sim = 500)
	# boot.p.vals.natl.max.wind <- summarise_p_values("NATL", "storm.duration", "max.wind", bootstrap = T, n.sim = 500)
	# boot.p.vals.natl.mean.wind <- summarise_p_values("NATL", "storm.duration", "mean.wind", bootstrap = T, n.sim = 500)
	# boot.p.vals.natl.mean.sq.wind <- summarise_p_values("NATL", "storm.duration", "mean.sq.wind", bootstrap = T, n.sim = 500)

	# EPAC
	boot.p.vals.epac.pdi <- summarise_p_values("EPAC", "storm.duration", "storm.pdi", bootstrap = T, n.sim = 500)
	# boot.p.vals.epac.max.wind <- summarise_p_values("EPAC", "storm.duration", "max.wind", bootstrap = T, n.sim = 500)
	# boot.p.vals.epac.mean.wind <- summarise_p_values("EPAC", "storm.duration", "mean.wind", bootstrap = T, n.sim = 500)
	# boot.p.vals.epac.mean.sq.wind <- summarise_p_values("EPAC", "storm.duration", "mean.sq.wind", bootstrap = T, n.sim = 500)
}

# Permutation test for developing systems
if (compute.flag && bootstrap.flag) {
	# NATL
	boot.p.vals.natl.pdi.ds <- summarise_p_values("NATL", "storm.duration", "storm.pdi", 33, bootstrap = T, n.sim = 500)
	# boot.p.vals.natl.max.wind.ds <- summarise_p_values("NATL", "storm.duration", "max.wind", 33, bootstrap = T, n.sim = 500)
	# boot.p.vals.natl.mean.wind.ds <- summarise_p_values("NATL", "storm.duration", "mean.wind", 33, bootstrap = T, n.sim = 500)
	# boot.p.vals.natl.mean.sq.wind.ds <- summarise_p_values("NATL", "storm.duration", "mean.sq.wind", 33, bootstrap = T, n.sim = 500)

	# EPAC
	boot.p.vals.epac.pdi.ds <- summarise_p_values("EPAC", "storm.duration", "storm.pdi", 33, bootstrap = T, n.sim = 500)
	# boot.p.vals.epac.max.wind.ds <- summarise_p_values("EPAC", "storm.duration", "max.wind", 33, bootstrap = T, n.sim = 500)
	# boot.p.vals.epac.mean.wind.ds <- summarise_p_values("EPAC", "storm.duration", "mean.wind", 33, bootstrap = T, n.sim = 500)
	# boot.p.vals.epac.mean.sq.wind.ds <- summarise_p_values("EPAC", "storm.duration", "mean.sq.wind", 33, bootstrap = T, n.sim = 500)

	# Save final time
	final.time <- Sys.time()
}

if (compute.flag && bootstrap.flag) {
	# Time goes as 0.007*n^2
	boot.elapsed.time <- final.time - init.time
	system("notify-send 'Finished calculations' 'Get back to work!' -i rstudio -u critical")
}

# Tidy p-values in a list ----------------------------------

if (compute.flag && !bootstrap.flag) {
	# Group data frames into a list
	rm(p.values.list)
	p.values.list <- lapply(ls(patt='^p.vals.'), get)
	# rm(list=ls(pattern="^p.vals."))
	saveRDS(p.values.list, "slopes_p_values.rds")
}

if (compute.flag && bootstrap.flag) {
	# Group data frames into a list
	rm(boot.p.values.list)
	boot.p.values.list <- lapply(ls(patt='^boot.p.vals.'), get)
	# rm(list=ls(pattern="^boot.p.vals."))
	saveRDS(boot.p.values.list, "slopes_p_values_boot.rds")
}

# Analyse p-values -----------------------------------------

# Print regressions with p-value <= alpha
explore_p_values(p.values.list, 0.05)
explore_p_values(boot.p.values.list, 0.05)

# NATL (all storms)
p.values.list[lapply(purrr::map(p.values.list, ~dplyr::filter(.x, basin == "NATL", min.speed == 0)), nrow) > 0]

# NATL (all storms)
p.values.list[lapply(purrr::map(p.values.list, ~dplyr::filter(.x, basin == "EPAC", min.speed == 0)), nrow) > 0]

# NATL (developing systems)
p.values.list[lapply(purrr::map(p.values.list, ~dplyr::filter(.x, basin == "NATL", min.speed == 33)), nrow) > 0]

# EPAC (developing systems)
p.values.list[lapply(purrr::map(p.values.list, ~dplyr::filter(.x, basin == "EPAC", min.speed == 33)), nrow) > 0]
