# Code to perform different statistical tests to analise the regression coefficients of PDI vs duration
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

# Load objects from disk -----------------------------------

if (!compute.flag) {
	lm.coefs.list <- readRDS("objects/regression_lm_coefs_list.rds")
	factors.lm.coefs.list <- readRDS("objects/regression_lm_coefs_fact_list.rds")
}

# Confidence interval for all storms -----------------------

if (compute.flag) {
	# NATL
	lm.coefs.natl.pdi <- summarise_lm_coefs("NATL", "storm.duration", "storm.pdi")
	lm.coefs.natl.max.wind <- summarise_lm_coefs("NATL", "storm.duration", "max.wind")
	lm.coefs.natl.mean.wind <- summarise_lm_coefs("NATL", "storm.duration", "mean.wind")
	lm.coefs.natl.mean.sq.wind <- summarise_lm_coefs("NATL", "storm.duration", "mean.sq.wind")

	# EPAC
	lm.coefs.epac.pdi <- summarise_lm_coefs("EPAC", "storm.duration", "storm.pdi")
	lm.coefs.epac.max.wind <- summarise_lm_coefs("EPAC", "storm.duration", "max.wind")
	lm.coefs.epac.mean.wind <- summarise_lm_coefs("EPAC", "storm.duration", "mean.wind")
	lm.coefs.epac.mean.sq.wind <- summarise_lm_coefs("EPAC", "storm.duration", "mean.sq.wind")
}


# Confidence interval for developing systems ---------------

if (compute.flag) {
	# NATL
	lm.coefs.natl.pdi.ds <- summarise_lm_coefs("NATL", "storm.duration", "storm.pdi", 33)
	lm.coefs.natl.max.wind.ds <- summarise_lm_coefs("NATL", "storm.duration", "max.wind", 33)
	lm.coefs.natl.mean.wind.ds <- summarise_lm_coefs("NATL", "storm.duration", "mean.wind", 33)
	lm.coefs.natl.mean.sq.wind.ds <- summarise_lm_coefs("NATL", "storm.duration", "mean.sq.wind", 33)

	# EPAC
	lm.coefs.epac.pdi.ds <- summarise_lm_coefs("EPAC", "storm.duration", "storm.pdi", 33)
	lm.coefs.epac.max.wind.ds <- summarise_lm_coefs("EPAC", "storm.duration", "max.wind", 33)
	lm.coefs.epac.mean.wind.ds <- summarise_lm_coefs("EPAC", "storm.duration", "mean.wind", 33)
	lm.coefs.epac.mean.sq.wind.ds <- summarise_lm_coefs("EPAC", "storm.duration", "mean.sq.wind", 33)
}


# Factors of CI --------------------------------------------

# Confidence interval for all storms -----------------------

if (compute.flag) {
	# NATL
	fact.natl.pdi <- summarise_lm_coefs("NATL", "storm.duration", "storm.pdi", metrics = "factor")
	fact.natl.max.wind <- summarise_lm_coefs("NATL", "storm.duration", "max.wind", metrics = "factor")
	fact.natl.mean.wind <- summarise_lm_coefs("NATL", "storm.duration", "mean.wind", metrics = "factor")
	fact.natl.mean.sq.wind <- summarise_lm_coefs("NATL", "storm.duration", "mean.sq.wind", metrics = "factor")

	# EPAC
	fact.epac.pdi <- summarise_lm_coefs("EPAC", "storm.duration", "storm.pdi", metrics = "factor")
	fact.epac.max.wind <- summarise_lm_coefs("EPAC", "storm.duration", "max.wind", metrics = "factor")
	fact.epac.mean.wind <- summarise_lm_coefs("EPAC", "storm.duration", "mean.wind", metrics = "factor")
	fact.epac.mean.sq.wind <- summarise_lm_coefs("EPAC", "storm.duration", "mean.sq.wind", metrics = "factor")
}


# Confidence interval for developing systems ---------------

if (compute.flag) {
	# NATL
	fact.natl.pdi.ds <- summarise_lm_coefs("NATL", "storm.duration", "storm.pdi", 33, metrics = "factor")
	fact.natl.max.wind.ds <- summarise_lm_coefs("NATL", "storm.duration", "max.wind", 33, metrics = "factor")
	fact.natl.mean.wind.ds <- summarise_lm_coefs("NATL", "storm.duration", "mean.wind", 33, metrics = "factor")
	fact.natl.mean.sq.wind.ds <- summarise_lm_coefs("NATL", "storm.duration", "mean.sq.wind", 33, metrics = "factor")

	# EPAC
	fact.epac.pdi.ds <- summarise_lm_coefs("EPAC", "storm.duration", "storm.pdi", 33, metrics = "factor")
	fact.epac.max.wind.ds <- summarise_lm_coefs("EPAC", "storm.duration", "max.wind", 33, metrics = "factor")
	fact.epac.mean.wind.ds <- summarise_lm_coefs("EPAC", "storm.duration", "mean.wind", 33, metrics = "factor")
	fact.epac.mean.sq.wind.ds <- summarise_lm_coefs("EPAC", "storm.duration", "mean.sq.wind", 33, metrics = "factor")
}


# Scatterplots (all storms) --------------------------------

# # NATL
# plot_scatterplot("NATL", "storm.duration", "storm.pdi")
# plot_scatterplot("NATL", "storm.duration", "max.wind")
# plot_scatterplot("NATL", "storm.duration", "mean.wind")
# plot_scatterplot("NATL", "storm.duration", "mean.sq.wind")
#
# # EPAC
# plot_scatterplot("EPAC", "storm.duration", "storm.pdi")
# plot_scatterplot("EPAC", "storm.duration", "max.wind")
# plot_scatterplot("EPAC", "storm.duration", "mean.wind")
# plot_scatterplot("EPAC", "storm.duration", "mean.sq.wind")


# Scatterplots (developing systems) ------------------------

# # NATL
# plot_scatterplot("NATL", "storm.duration", "storm.pdi", 33)
# plot_scatterplot("NATL", "storm.duration", "max.wind", 33)
# plot_scatterplot("NATL", "storm.duration", "mean.wind", 33)
# plot_scatterplot("NATL", "storm.duration", "mean.sq.wind", 33)
#
# # EPAC
# plot_scatterplot("EPAC", "storm.duration", "storm.pdi", 33)
# plot_scatterplot("EPAC", "storm.duration", "max.wind", 33)
# plot_scatterplot("EPAC", "storm.duration", "mean.wind", 33)
# plot_scatterplot("EPAC", "storm.duration", "mean.sq.wind", 33)



# Aggregate results ----------------------------------------

# Group data frames into a list
if (compute.flag) {
	# rm(lm.coefs.list)
	lm.coefs.list <- lapply(ls(patt='^lm.coefs.'), get)
	# rm(factors.lm.coefs.list)
	factors.lm.coefs.list <- lapply(ls(patt='^fact.'), get)
	saveRDS(lm.coefs.list, "objects/regression_lm_coefs_list.rds")
	saveRDS(factors.lm.coefs.list, "objects/regression_lm_coefs_fact_list.rds")
	# rm(list=ls(pattern="^lm.coefs.epac"))
	# rm(list=ls(pattern="^lm.coefs.natl"))
}

# NATL (all storms)
lm.coefs.list.natl <- lm.coefs.list[lapply(purrr::map(lm.coefs.list, ~dplyr::filter(.x, basin == "NATL", min.speed == 0)), nrow) > 0]

# NATL (all storms)
lm.coefs.list.epac <- lm.coefs.list[lapply(purrr::map(lm.coefs.list, ~dplyr::filter(.x, basin == "EPAC", min.speed == 0)), nrow) > 0]

# NATL (developing systems)
lm.coefs.list.natl.ds <- lm.coefs.list[lapply(purrr::map(lm.coefs.list, ~dplyr::filter(.x, basin == "NATL", min.speed == 33)), nrow) > 0]

# EPAC (developing systems)
lm.coefs.list.epac.ds <- lm.coefs.list[lapply(purrr::map(lm.coefs.list, ~dplyr::filter(.x, basin == "EPAC", min.speed == 33)), nrow) > 0]


# Summarise regression coefficients ------------------------

# NATL basin
lm.coefs.natl.pdi.ds <- lm.coefs.list.natl.ds[[4]]


cbind(lm.coefs.natl.pdi.ds[10:9], lm.coefs.natl.pdi.ds[1:2], round(lm.coefs.natl.pdi.ds[c(5,6,3,4,7,8)], 2)) %>% dplyr::arrange(metric)

# T statistics

rbind(
	# PDI ~ duration OLS
	get_t_statistics(
		coefs.low =  as.numeric(lm.coefs.list.natl.ds[[4]][1,3:7]),
		coefs.high = as.numeric(lm.coefs.list.natl.ds[[4]][3,3:7])
	),
	# Duration ~ PDI OLS
	get_t_statistics(
		coefs.low =  as.numeric(lm.coefs.list.natl.ds[[4]][5,3:7]),
		coefs.high = as.numeric(lm.coefs.list.natl.ds[[4]][7,3:7])
	)
)

rbind(
	# PDI ~ duration Bootstrap
	get_t_statistics(
		coefs.low =  as.numeric(lm.coefs.list.natl.ds[[4]][2,3:7]),
		coefs.high = as.numeric(lm.coefs.list.natl.ds[[4]][4,3:7])
	),
	# Duration ~ PDI Bootstrap
	get_t_statistics(
		coefs.low =  as.numeric(lm.coefs.list.natl.ds[[4]][6,3:7]),
		coefs.high = as.numeric(lm.coefs.list.natl.ds[[4]][8,3:7])
	)
)


# EPAC basin
lm.coefs.epac.pdi.ds <- lm.coefs.list.epac.ds[[4]]

cbind(lm.coefs.epac.pdi.ds[10:9], lm.coefs.epac.pdi.ds[1:2], round(lm.coefs.epac.pdi.ds[c(5,6,3,4,7,8)], 2)) %>% dplyr::arrange(metric)


# T statistics (EPAC)

rbind(
	# PDI ~ duration OLS
	get_t_statistics(
		coefs.low =  as.numeric(lm.coefs.list.epac.ds[[4]][1,3:7]),
		coefs.high = as.numeric(lm.coefs.list.epac.ds[[4]][3,3:7])
	),
	# Duration ~ PDI OLS
	get_t_statistics(
		coefs.low =  as.numeric(lm.coefs.list.epac.ds[[4]][5,3:7]),
		coefs.high = as.numeric(lm.coefs.list.epac.ds[[4]][7,3:7])
	)
)

rbind(
	# PDI ~ duration Bootstrap
	get_t_statistics(
		coefs.low =  as.numeric(lm.coefs.list.epac.ds[[4]][2,3:7]),
		coefs.high = as.numeric(lm.coefs.list.epac.ds[[4]][4,3:7])
	),
	# Duration ~ PDI Bootstrap
	get_t_statistics(
		coefs.low =  as.numeric(lm.coefs.list.epac.ds[[4]][6,3:7]),
		coefs.high = as.numeric(lm.coefs.list.epac.ds[[4]][8,3:7])
	)
)
