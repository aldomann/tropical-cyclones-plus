# Code to analyse the residuals in the linear model
# Author: Alfredo Hernández <aldomann.designs@gmail.com>


# Libraries ------------------------------------------------
library(qqplotr)

# Source base code -----------------------------------------
source("regression_base.R")

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

# boot.low <- perform_bootstrap(data.low$storm.duration, data.low$storm.pdi, 5000)
boot.low.stats <- boot.low[[1]]
boot.low.data <- boot.low[[2]]

# boot.high <- perform_bootstrap(data.high$storm.duration, data.high$storm.pdi, 5000)
boot.high.stats <- boot.high[[1]]
boot.high.data <- boot.high[[2]]


# Histograms of sample -------------------------------------

ggplot(boot.low.data) +
	geom_histogram(aes(x = slope), binwidth = 0.01)

ggplot(boot.low.data) +
	geom_histogram(aes(x = inter), binwidth = 0.025)

ggplot(boot.low.data) +
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


# QQ Plots (residuals) -------------------------------------
plot_resid_qqplot <- function(fit) {
	res.data <-  tibble(resid = resid(fit))
	gg <- ggplot(res.data, aes( sample = resid )) +
		stat_qq_line() +
		stat_qq_point(shape = 1, size = 2.5) +
		labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
		theme_bw()

	return(gg)
}

plot_resid_qqplot(fit.low)
plot_resid_qqplot(fit.high)

get_boot_residuals <- function(col.x, col.y, boot.stats) {
	slope <- boot.stats$slope
	inter <- boot.stats$inter

	data <- tibble(x = log10(col.x),
								 y = log10(col.y))

	data <- data %>%
		mutate(
			y.pred = inter + x * slope,
			resid = y - y.pred)

	return(data)
}

boot.low.resid <- get_boot_residuals(data.low$storm.duration, data.low$storm.pdi, boot.low.stats)

plot_qqplot(boot.low.resid[["resid"]], "Residuals")



# Q-Q Plots (any variable) ---------------------------------

plot_qqplot <- function(col, name) {
	var.data <-  tibble(variable = col)
	gg <- ggplot(var.data, aes( sample = variable )) +
		stat_qq_line() +
		stat_qq_point(shape = 1, size = 2.5) +
		labs(x = "Theoretical Quantiles", y = name) +
		theme_bw()

	return(gg)
}

plot_qqplot(boot.low.data[["slope"]], "Slope")
plot_qqplot(boot.low.data[["inter"]], "Intercept")
