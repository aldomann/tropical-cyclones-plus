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
	dplyr::filter(basin == "epac")

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
get_conf_interval <- function(df) {
	dur <- df$storm.duration
	pdi <- df$storm.pdi

	fit <- lm(log10(pdi) ~ log10(dur))
	theta <- summary(fit)$coefficients[2]
	sdtheta <- summary(fit)$coefficients[4]

	data <- cbind(log10(dur), log10(pdi))
	n <- nrow(data)

	nb <- 1000 # Number of simulations
	y <- seq(1, n)
	thetab <- numeric(nb)
	sdthetab <- numeric(nb)
	tb <- numeric(nb)

	# Loop for
	for (i in 1:nb) {
		yb <- sample(y, n, replace = T)
		fitb <- lm(data[yb, 2] ~ data[yb, 1])
		thetab[i] <- summary(fitb)$coefficients[2]
		sdthetab[i] <- summary(fitb)$coefficients[4]
		tb[i] <- (thetab[i] - theta) / sdthetab[i]
	}

	# The bootstrap-t 95% method
	lower.lim.theta <- theta + sdtheta * quantile(tb, 0.025)
	upper.lim.theta <- theta + sdtheta * quantile(tb, 0.975)

	# Simple Method
	a <- 2 * theta - quantile(thetab, 0.975)
	b <- 2 * theta - quantile(thetab, 0.025)

	# Quantile method
	quan <- quantile(thetab, c(0.025, 0.975))

	# return(c(a, b))
}

get_conf_interval(pdi.natl.low)
