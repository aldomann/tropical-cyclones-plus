# Code to analyse the residuals in the linear model
# Author: Alfredo Hernández <aldomann.designs@gmail.com>


# Libraries ------------------------------------------------
library(qqplotr)

# Source base code -----------------------------------------
source("slopes_base.R")

# Get RAW data ---------------------------------------------

pdi.all <- as_tibble(data.table::fread('data/hurdat2-hadisst-1966-2016_pdis.csv')) %>%
	mutate(storm.duration = measurements::conv_unit(storm.duration, "sec", "hr"))

pdi.natl <- pdi.all %>%
	dplyr::filter(basin == "NATL")
pdi.epac <- pdi.all %>%
	dplyr::filter(basin == "EPAC")

# Clean data -----------------------------------------------

data.low <- pdi.natl %>%
	dplyr::filter(sst.class == "low") %>%
	dplyr::filter(max.wind > 33)

data.high <- pdi.natl %>%
	dplyr::filter(sst.class == "high") %>%
	dplyr::filter(max.wind > 33)



# Linear models --------------------------------------------

fit.low <- lm(data = data.low, log10(storm.pdi) ~ log10(storm.duration))
fit.high <- lm(data = data.high, log10(storm.pdi) ~ log10(storm.duration))

boot.low <- perform_bootstrap(data.low$storm.duration, data.low$storm.pdi, 5000, full = T)
boot.low.stats <- perform_bootstrap(data.low$storm.duration, data.low$storm.pdi, 5000)

boot.high <- perform_bootstrap(data.high$storm.duration, data.high$storm.pdi, 5000, full = T)
boot.high.stats <- perform_bootstrap(data.high$storm.duration, data.high$storm.pdi, 5000)


# Histograms of sample -------------------------------------

ggplot(boot.low) +
	geom_histogram(aes(x = slope), binwidth = 0.01)

ggplot(boot.low) +
	geom_histogram(aes(x = inter), binwidth = 0.025)

ggplot(boot.low) +
	geom_histogram(aes(x = r2), binwidth = 0.005)



# Residuals vs fitted values -------------------------------

plot_resid_vs_fitted <- function(fit) {
	data <- tibble(fitted = fitted(fit),
								 resid = resid(fit))

	gg <- ggplot(data) +
		geom_point(aes(x = fitted, y = resid),
							 shape = 1, size = 2.5) +
		labs(x = "Fitted values", y = "Residuals") +
		theme_bw()

	return(gg)
}

plot_resid_vs_fitted(fit.low)
plot_resid_vs_fitted(fit.high)


# QQ Plots -------------------------------------------------
plot_qqplot <- function(fit) {
	res.data <-  tibble(resid = resid(fit))
	gg <- ggplot(res.data, aes( sample = resid )) +
		stat_qq_line() +
		stat_qq_point(shape = 1, size = 2.5) +
		labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
		theme_bw()

	return(gg)
}

plot_qqplot(fit.low)
plot_qqplot(fit.high)
