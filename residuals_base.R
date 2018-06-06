# Code to analyse the residuals in the linear model
# Author: Alfredo Hernández <aldomann.designs@gmail.com>


# Libraries ------------------------------------------------
library(qqplotr)
library(quantreg)

# Source base code -----------------------------------------
# source("regression_base.R")

# Residuals vs fitted values -------------------------------

plot_resid_vs_fitted <- function(fit) {
	data <- tibble(fitted = fitted(fit),
								 resid = resid(fit))

	smooth <-  as_tibble(lowess(data))

	gg <- ggplot(data) +
		aes(x = fitted, y = resid) +
		geom_point(shape = 1, size = 1.5) +
		geom_line(data = smooth, aes(x = x, y = y), colour = "red")+
		geom_hline(yintercept = 0, linetype = "dashed") +
		labs(x = "Fitted values", y = "Residuals") +
		theme_bw()

	return(gg)
}

plot_resid_vs_fitted_from_data <- function(data, formula) {

	fit <- lm(data = data, eval(formula))
	fit.data <- tibble(fitted = fitted(fit),
										 resid = resid(fit))

	alpha <- 0.05
	smooth <-  as_tibble(lowess(fit.data))

	gg <- ggplot(fit.data) +
		aes(x = fitted, y = resid) +
		geom_point(shape = 1, size = 1.5) +
		geom_line(data = smooth, aes(x = x, y = y), colour = "red") +
		geom_quantile(quantiles = c(alpha, 1 - alpha), method = "rq", colour = "blue") +
		geom_hline(yintercept = 0, linetype = "dashed") +
		labs(x = "Fitted values", y = "Residuals") +
		theme_bw()

	return(gg)
}


# QQ Plots (residuals) -------------------------------------
plot_resid_qqplot <- function(fit) {
	res.data <-  tibble(resid = resid(fit))
	gg <- ggplot(res.data, aes( sample = resid )) +
		stat_qq_line() +
		stat_qq_point(shape = 1, size = 1.5) +
		labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
		theme_bw()

	return(gg)
}

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


# Q-Q Plots (any variable) ---------------------------------

plot_qqplot <- function(col, name) {
	var.data <-  tibble(variable = col)
	gg <- ggplot(var.data, aes( sample = variable )) +
		stat_qq_line() +
		stat_qq_point(shape = 1, size = 1.5) +
		labs(x = "Theoretical Quantiles", y = name) +
		theme_bw()

	return(gg)
}
