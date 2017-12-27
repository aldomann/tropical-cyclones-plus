# Code to perform different statistical tests to analise the slopes of PDI vs duration
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Libraries ------------------------------------------------
library(tidyverse)
library(lubridate)
library(data.table)
library(measurements) # Convert units

# Get RAW data ---------------------------------------------

pdi.all <- fread('data/hurdat2-hadisst-1966-2016.csv')
pdi.natl <- pdi.all %>%
	dplyr::filter(basin == "NATL")
pdi.epac <- pdi.all %>%
	dplyr::filter(basin == "EPAC")

# Clean data -----------------------------------------------

# North Atlantic
pdi.natl.low <- pdi.natl %>%
	dplyr::filter(sst.class == "low")

pdi.natl.high <- pdi.natl %>%
	dplyr::filter(sst.class == "high")

# East Pacific
pdi.epac.low <- pdi.epac %>%
	dplyr::filter(sst.class == "low")

pdi.epac.high <- pdi.epac %>%
	dplyr::filter(sst.class == "high")

# Test plot ------------------------------------------------

ggplot() +
	aes(x = storm.duration, y = storm.pdi) +
	geom_point(data = pdi.natl.low, aes(colour = 'low')) +
	geom_point(data = pdi.natl.high, aes(colour = 'high')) +
	scale_x_log10() +
	scale_y_log10()

# Confidence interval --------------------------------------

get_conf_interval <- function(df, sst.class) {
	df <- df %>%
		dplyr::filter(sst.class == sst.class)

	dur <- df$storm.duration
	pdi <- df$storm.pdi

	fit <- lm(log10(pdi) ~ log10(dur))
	slope <- summary(fit)$coefficients[2]
	slope.sd <- summary(fit)$coefficients[4]

	data <- cbind(log10(dur), log10(pdi))
	n <- nrow(data)

	nsim <- 1000 # Number of simulations
	data.seq <- seq(1, n)
	slope.sim <- numeric(nsim)
	slope.sd.sim <- numeric(nsim)
	t.value.sim <- numeric(nsim)

	# Perform the simulation
	for (i in 1:nsim) {
		sim.sample <- sample(data.seq, n, replace = T)
		fit.value.sim <- lm(data[sim.sample, 2] ~ data[sim.sample, 1])
		slope.sim[i] <- summary(fit.value.sim)$coefficients[2]
		slope.sd.sim[i] <- summary(fit.value.sim)$coefficients[4]
		t.value.sim[i] <- (slope.sim[i] - slope) / slope.sd.sim[i]
	}

	# The bootstrap-t 95% method
	boot.low <- slope + slope.sd * quantile(t.value.sim, 0.025)
	boot.upp <- slope + slope.sd * quantile(t.value.sim, 0.975)
	boot.sd <- (boot.upp - boot.low) / 2
	boot.slope <- boot.upp - boot.sd

	# Simple Method
	simp.low <- 2 * slope - quantile(slope.sim, 0.975)
	simp.upp <- 2 * slope - quantile(slope.sim, 0.025)
	simp.sd <- (simp.upp - simp.low) / 2
	simp.slope <- simp.upp - simp.sd

	# Quantile method
	quan.low <- quantile(slope.sim, 0.025)
	quan.upp <- quantile(slope.sim, 0.975)
	quan.sd <- (quan.upp - quan.low) / 2
	quan.slope <- quan.upp - quan.sd

	results.df <- data.frame(
		method = c("bootstrap-t", "simple", "quantile"),
		slope = c(boot.slope, simp.slope, quan.slope),
		sd = c(boot.sd, simp.sd, quan.sd)
		)

	return(results.df)
}

ci.natl.low <- get_conf_interval(pdi.natl, "low")
ci.natl.high <- get_conf_interval(pdi.natl, "high")

ci.epac.low <- get_conf_interval(pdi.epac, "low")
ci.epac.high <- get_conf_interval(pdi.epac, "high")
