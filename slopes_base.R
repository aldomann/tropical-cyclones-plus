# Base code to perform different statistical tests to analise the slopes of PDI vs duration
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Libraries ------------------------------------------------
library(tidyverse)
library(lubridate)
library(data.table)

# Confidence interval --------------------------------------

get_conf_interval <- function(df, sst.class) {
	# Filter and clean data
	df <- df %>%
		dplyr::filter(sst.class == sst.class)
	dur <- df$storm.duration
	pdi <- df$storm.pdi

	# Construct data
	data <- cbind(log10(dur), log10(pdi))
	n <- nrow(data)

	# Linear regression
	fit <- lm(log10(pdi) ~ log10(dur))
	slope <- summary(fit)$coefficients[2]
	slope.sd <- summary(fit)$coefficients[4]

	# Prepare variables for the simulation
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

# Permutation test -----------------------------------------

do_permutation_test <- function(df) {
	# Filter and clean data (low SST)
	df.low <- df %>%
		dplyr::filter(sst.class == "low")
	dur.low <- df.low$storm.duration
	pdi.low <- df.low$storm.pdi

	# Filter and clean data (high SST)
	df.high <- df %>%
		dplyr::filter(sst.class == "high")
	dur.high <- df.high$storm.duration
	pdi.high <- df.high$storm.pdi

	# Linear regressions statistics
	fit.low <- lm(log10(pdi.low) ~ log10(dur.low))
	fit.high <- lm(log10(pdi.high) ~ log10(dur.high))

	slope.low <- summary(fit.low)$coefficients[2]
	slope.high <- summary(fit.high)$coefficients[2]

	stat.true <- slope.high - slope.low

	# Construct data
	data.high <-cbind(log10(dur.high), log10(pdi.high))
	data.low <-cbind(log10(dur.low), log10(pdi.low))
	data.all <- rbind (data.high, data.low)

	n.low <- nrow(data.low)
	n.high <- nrow(data.high)
	n.all <- n.low + n.high

	# Prepare variables for the test
	n.sim <- 10000  # Number of simulations
	stat.sim <- numeric(n.sim)
	count <- 0

	# Perform the permutation test
	for (i in 1:n.sim){
		dur.sample <- sample (data.all[,1], n.all)
		pdi.sample <- sample (data.all[,2], n.all)
		data.sample <- cbind(dur.sample, pdi.sample)

		low <- cbind(data.sample[1:n.low, 1], data.sample[1:n.low, 2])
		x <- n.low + 1
		high <- cbind(data.sample[x:n.all, 1], data.sample[x:n.all, 2])

		fit.lowp <- lm(log10(low[,1]) ~ log10(low[,2]))
		fit.highp <- lm(log10(high[,1]) ~ log10(high[,2]))
		slope.lowp <- summary(fit.lowp)$coefficients[2]
		slope.highp <- summary(fit.highp)$coefficients[2]
		stat.sim[i] <- slope.highp - slope.lowp

		if (stat.sim[i] > stat.true) {
			count <- count + 1
		}
	}
	# Calculate p-value and its error
	p.value <- count/n.sim
	p.value.err <- 1.96 * (sqrt((p.value - (p.value)^2) / n.sim))

	return( c(p.value, p.value.err) )
}

