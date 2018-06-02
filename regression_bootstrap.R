# Code to perform different statistical tests to analise the regression coefficients of PDI vs duration
# Author: Alfredo Hernández <aldomann.designs@gmail.com>


# Source base code -----------------------------------------
source("regression_base.R")
# load("regression_analysis.RData")


# Get RAW data ---------------------------------------------

pdi.all <- as_tibble(data.table::fread('data/hurdat2-hadisst-1966-2016_pdis.csv')) %>%
	mutate(storm.duration = measurements::conv_unit(storm.duration, "sec", "hr"))

pdi.natl <- pdi.all %>%
	dplyr::filter(basin == "NATL")
pdi.epac <- pdi.all %>%
	dplyr::filter(basin == "EPAC")

compute.flag <- F

# Load objects from disk -----------------------------------

if (!compute.flag) {
	ci.list <- readRDS("regression_ci_list.rds")
	factors.ci.list <- readRDS("regression_ci_fact_list.rds")
}


# Confidence interval for all storms -----------------------

if (compute.flag) {
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
}


# Confidence interval for developing systems ---------------

if (compute.flag) {
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
}


# Factors of CI --------------------------------------------

# Confidence interval for all storms -----------------------

alfR::lok_regar(if (compute.flag) {
	# NATL
	fact.natl.pdi <- summarise_conf_intervals_factors("NATL", "storm.duration", "storm.pdi")
	fact.natl.max.wind <- summarise_conf_intervals_factors("NATL", "storm.duration", "max.wind")
	fact.natl.mean.wind <- summarise_conf_intervals_factors("NATL", "storm.duration", "mean.wind")
	fact.natl.mean.sq.wind <- summarise_conf_intervals_factors("NATL", "storm.duration", "mean.sq.wind")

	# EPAC
	fact.epac.pdi <- summarise_conf_intervals_factors("EPAC", "storm.duration", "storm.pdi")
	fact.epac.max.wind <- summarise_conf_intervals_factors("EPAC", "storm.duration", "max.wind")
	fact.epac.mean.wind <- summarise_conf_intervals_factors("EPAC", "storm.duration", "mean.wind")
	fact.epac.mean.sq.wind <- summarise_conf_intervals_factors("EPAC", "storm.duration", "mean.sq.wind")
})


# Confidence interval for developing systems ---------------

alfR::lok_regar(if (compute.flag) {
	# NATL
	fact.natl.pdi.ds <- summarise_conf_intervals_factors("NATL", "storm.duration", "storm.pdi", 33)
	fact.natl.max.wind.ds <- summarise_conf_intervals_factors("NATL", "storm.duration", "max.wind", 33)
	fact.natl.mean.wind.ds <- summarise_conf_intervals_factors("NATL", "storm.duration", "mean.wind", 33)
	fact.natl.mean.sq.wind.ds <- summarise_conf_intervals_factors("NATL", "storm.duration", "mean.sq.wind", 33)

	# EPAC
	fact.epac.pdi.ds <- summarise_conf_intervals_factors("EPAC", "storm.duration", "storm.pdi", 33)
	fact.epac.max.wind.ds <- summarise_conf_intervals_factors("EPAC", "storm.duration", "max.wind", 33)
	fact.epac.mean.wind.ds <- summarise_conf_intervals_factors("EPAC", "storm.duration", "mean.wind", 33)
	fact.epac.mean.sq.wind.ds <- summarise_conf_intervals_factors("EPAC", "storm.duration", "mean.sq.wind", 33)
})




# Scatterplots (all storms) --------------------------------

# NATL
plot_scatterplot("NATL", "storm.duration", "storm.pdi")
plot_scatterplot("NATL", "storm.duration", "max.wind")
plot_scatterplot("NATL", "storm.duration", "mean.wind")
plot_scatterplot("NATL", "storm.duration", "mean.sq.wind")

# EPAC
plot_scatterplot("EPAC", "storm.duration", "storm.pdi")
plot_scatterplot("EPAC", "storm.duration", "max.wind")
plot_scatterplot("EPAC", "storm.duration", "mean.wind")
plot_scatterplot("EPAC", "storm.duration", "mean.sq.wind")


# Scatterplots (developing systems) ------------------------

# NATL
plot_scatterplot("NATL", "storm.duration", "storm.pdi", 33)
plot_scatterplot("NATL", "storm.duration", "max.wind", 33)
plot_scatterplot("NATL", "storm.duration", "mean.wind", 33)
plot_scatterplot("NATL", "storm.duration", "mean.sq.wind", 33)

# EPAC
plot_scatterplot("EPAC", "storm.duration", "storm.pdi", 33)
plot_scatterplot("EPAC", "storm.duration", "max.wind", 33)
plot_scatterplot("EPAC", "storm.duration", "mean.wind", 33)
plot_scatterplot("EPAC", "storm.duration", "mean.sq.wind", 33)


# Summarise CI data frames ---------------------------------

# Group data frames into a list
if (compute.flag) {
	rm(ci.list)
	ci.list <- lapply(ls(patt='^ci.'), get)
	factors.ci.list <- lapply(ls(patt='^fact.'), get)
	saveRDS(ci.list, "regression_ci_list.rds")
	saveRDS(factors.ci.list, "regression_ci_fact_list.rds")
	# rm(list=ls(pattern="^ci.epac"))
	# rm(list=ls(pattern="^ci.natl"))
}

# NATL (all storms)
ci.list[lapply(purrr::map(ci.list, ~dplyr::filter(.x, basin == "NATL", min.speed == 0)), nrow) > 0]

# NATL (all storms)
ci.list[lapply(purrr::map(ci.list, ~dplyr::filter(.x, basin == "EPAC", min.speed == 0)), nrow) > 0]

# NATL (developing systems)
ci.list[lapply(purrr::map(ci.list, ~dplyr::filter(.x, basin == "NATL", min.speed == 33)), nrow) > 0]

# EPAC (developing systems)
ci.list[lapply(purrr::map(ci.list, ~dplyr::filter(.x, basin == "EPAC", min.speed == 33)), nrow) > 0]


# Compare CI methods ---------------------------------------

compare_ci_methods(factors.ci.list)
