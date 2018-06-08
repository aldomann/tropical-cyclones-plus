# Base code to perform different statistical tests to analise the regression coefficients of PDI vs duration
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Libraries ------------------------------------------------
library(tidyverse)
library(lubridate)


# Non-parametric bootstrap ---------------------------------

perform_bootstrap <- function(col.x, col.y, n.sim = 500) {
	# Construct data
	data <- cbind(log10(col.x), log10(col.y))
	n <- nrow(data)

	# Linear regression
	fit <- lm(data[,2] ~ data[,1])
	sum.fit <- summary(fit)
	slope <- sum.fit$coefficients[2]
	inter <- sum.fit$coefficients[1]
	r.squared <- sum.fit$r.squared

	# Prepare variables for the simulation
	# n.sim <- 500 # Number of simulations
	data.seq <- seq(1, n)
	slope.sim <- numeric(n.sim)
	inter.sim <- numeric(n.sim)
	r.squared.sim <- numeric(n.sim)

	# Perform the simulation
	for (i in 1:n.sim) {
		sim.sample <- sample(data.seq, n, replace = T)

		# Fit linear model
		fit.value.sim <- lm(data[sim.sample, 2] ~ data[sim.sample, 1])
		sum.fit.value.sim <- summary(fit.value.sim)
		# Slope
		slope.sim[i] <- sum.fit.value.sim$coefficients[2]
		# Intercept
		inter.sim[i] <- sum.fit.value.sim$coefficients[1]
		# R-Squared
		r.squared.sim[i] <- sum.fit.value.sim$r.squared
	}

	# Summarise results
	full.results.df <- tibble(
		slope = slope.sim,
		inter = inter.sim,
		r2 = r.squared.sim
	)

	results.df <- tibble(
		slope =    mean(slope.sim),
		slope.sd = sd(slope.sim),
		inter =    mean(inter.sim),
		inter.sd = sd(inter.sim),
		r2 =       mean(r.squared.sim),
		r2.sd =    sd(r.squared.sim)
	)

	return(list(results.df, full.results.df))
}


# Linear regression coefficients ---------------------------

get_lm_coefs <- function(df, class, var1, var2, min.speed = 0, n.sim = 1000) {
	get_r_error <- function(r2, n){
		r.std <- sqrt((1 - r2) / (n - 2))
		r2.std <- 2 * r2 * r.std
		return(r2.std)
	}

	# Filter and clean data
	df <- df %>%
		dplyr::filter(max.wind > min.speed) %>%
		dplyr::filter(sst.class == class)
	col.x <- df[,var1]
	col.y <- df[,var2]

	# Construct data
	data <- cbind(log10(col.x), log10(col.y))
	n <- nrow(data)

	# Linear regression
	fit <- lm(data[,2] ~ data[,1])
	sum.fit <- summary(fit)
	slope <- sum.fit$coefficients[2]
	slope.sd <- sum.fit$coefficients[4]
	inter <- sum.fit$coefficients[1]
	inter.sd <- sum.fit$coefficients[3]
	r.squared <- sum.fit$r.squared
	r.squared.sd <- get_r_error(r.squared, n) # NEW

	boot.results <- perform_bootstrap(col.x, col.y)[[1]]

	boot.slope <- boot.results$slope
	boot.slope.sd <- boot.results$slope.sd
	boot.inter <- boot.results$inter
	boot.inter.sd <- boot.results$inter.sd
	boot.r2 <- boot.results$r2
	boot.r2.sd <- boot.results$r2.sd

	# # Prepare variables for the simulation
	# # n.sim <- 1000 # Number of simulations
	# data.seq <- seq(1, n)
	# slope.sim <- numeric(n.sim)
	# slope.sd.sim <- numeric(n.sim)
	# inter.sim <- numeric(n.sim)
	# inter.sd.sim <- numeric(n.sim)
	# r.squared.sim <- numeric(n.sim)
	# t.value.slope.sim <- numeric(n.sim)
	# t.value.inter.sim <- numeric(n.sim)
	#
	# # Perform the simulation
	# for (i in 1:n.sim) {
	# 	sim.sample <- sample(data.seq, n, replace = T)
	#
	# 	# Fit linear model
	# 	fit.value.sim <- lm(data[sim.sample, 2] ~ data[sim.sample, 1])
	# 	sum.fit.value.sim <- summary(fit.value.sim)
	# 	# Slope
	# 	slope.sim[i] <- sum.fit.value.sim$coefficients[2]
	# 	slope.sd.sim[i] <- sum.fit.value.sim$coefficients[4]
	# 	# Intercept
	# 	inter.sim[i] <- sum.fit.value.sim$coefficients[1]
	# 	inter.sd.sim[i] <- sum.fit.value.sim$coefficients[3]
	# 	# R-Squared
	# 	r.squared.sim[i] <- sum.fit.value.sim$r.squared
	#
	# 	# t-values
	# 	t.value.slope.sim[i] <- (slope.sim[i] - slope) / slope.sd.sim[i]
	# 	t.value.inter.sim[i] <- (inter.sim[i] - inter) / inter.sd.sim[i]
	# }
	#
	# # Bootstrap-t 95% method (slopes)
	# boot.slope.low <- slope + slope.sd * quantile(t.value.slope.sim, 0.025)
	# boot.slope.upp <- slope + slope.sd * quantile(t.value.slope.sim, 0.975)
	# boot.slope.sd <- (boot.slope.upp - boot.slope.low) / 2
	# boot.slope <- boot.slope.upp - boot.slope.sd
	#
	# # Bootstrap-t 95% method (intercepts)
	# boot.inter.low <- inter + inter.sd * quantile(t.value.inter.sim, 0.025)
	# boot.inter.upp <- inter + inter.sd * quantile(t.value.inter.sim, 0.975)
	# boot.inter.sd <- (boot.inter.upp - boot.inter.low) / 2
	# boot.inter <- boot.inter.upp - boot.inter.sd
	#
	# # Estimation of the correlation coefficient
	# r.squared.sim <- r.squared.sim[order(r.squared.sim)]
	# boot.r.sq.low <- quantile(r.squared.sim, 0.025)
	# boot.r.sq.upp <- quantile(r.squared.sim, 0.975)
	# boot.r.sq.sd <- (boot.r.sq.upp - boot.r.sq.low) / 2
	# boot.r.sq <- boot.r.sq.upp - boot.r.sq.sd

	# Summarise results
	results.df <- tibble(
		metric      = c("ols",        "bootstrap",   "factor"),
		sst.class   = rep(class, 3),
		slope       = c(slope,        boot.slope,    boot.slope / slope),
		slope.sd    = c(slope.sd,     boot.slope.sd, boot.slope.sd / slope.sd),
		inter       = c(inter,        boot.inter,    boot.inter / inter),
		inter.sd    = c(inter.sd,     boot.inter.sd, boot.inter.sd / inter.sd),
		r2          = c(r.squared,    boot.r2,       boot.r2 / r.squared),
		r2.sd       = c(r.squared.sd, boot.r2.sd,    boot.r2.sd / r.squared.sd), # NEW
		dep.var     = rep(var2, 3),
		indep.var   = rep(var1, 3)
	)

	return(results.df)
}

summarise_lm_coefs <- function(basin, var1, var2, min.speed = 0,
															 n.sim = 1000, metrics = c("ols", "bootstrap")) {
	# Parse the basin PDI data frame
	basin.df <- eval(parse(text=paste("pdi.", tolower(basin), sep = "")))

	# var2 ~ var1 regression (y ~ x)
	ci.yx.low <-  get_lm_coefs(basin.df, "low",  var1, var2, min.speed, n.sim)
	ci.yx.high <- get_lm_coefs(basin.df, "high", var1, var2, min.speed, n.sim)

	# var1 ~ var2 regression (x ~ y)
	ci.xy.low <-  get_lm_coefs(basin.df, "low",  var2, var1, min.speed)
	ci.xy.high <- get_lm_coefs(basin.df, "high", var2, var1, min.speed)

	# Construct summarised data frame
	results <- cbind(
		rbind(ci.yx.low %>% filter(metric %in% metrics),
					ci.yx.high %>% filter(metric %in% metrics),
					ci.xy.low %>% filter(metric %in% metrics),
					ci.xy.high %>% filter(metric %in% metrics)),
		basin = toupper(basin), min.speed = min.speed
	)

	return(results)
}


# Permutation test -----------------------------------------

do_permutation_test <- function(df, var1, var2, min.speed = 0, bootstrap = F, n.sim = 5000, n.boot = 50 ) {
	# Filter max wind speed
	df <- df %>%
		dplyr::filter(max.wind > min.speed)

	# Filter and clean data (low SST)
	df.low <- df %>%
		dplyr::filter(sst.class == "low")
	col.x.low <- df.low[[var1]]
	col.y.low <- df.low[[var2]]

	# Filter and clean data (high SST)
	df.high <- df %>%
		dplyr::filter(sst.class == "high")
	col.x.high <- df.high[[var1]]
	col.y.high <- df.high[[var2]]

	if (!bootstrap) {
		# Linear regressions statistics (standard)
		fit.low <- lm(log10(col.y.low) ~ log10(col.x.low))
		sum.fit.low <- summary(fit.low)
		fit.high <- lm(log10(col.y.high) ~ log10(col.x.high))
		sum.fit.high <- summary(fit.high)

		# True slopes
		slope.low <- sum.fit.low$coefficients[2]
		slope.sd.low <- sum.fit.low$coefficients[4]
		slope.high <- sum.fit.high$coefficients[2]
		slope.sd.high <- sum.fit.high$coefficients[4]

		# True intercepts
		inter.low <- sum.fit.low$coefficients[1]
		inter.sd.low <- sum.fit.low$coefficients[3]
		inter.high <- sum.fit.high$coefficients[1]
		inter.sd.high <- sum.fit.high$coefficients[3]

		# True R Squared coefficients
		r.sqr.low <- sum.fit.low$r.squared
		r.sqr.high <- sum.fit.high$r.squared
	} else if (bootstrap) {
		# True bootstrap statistics
		true.boot.lm.high <- perform_bootstrap(col.x.high, col.y.high, n.boot)
		true.boot.lm.low <- perform_bootstrap(col.x.low, col.y.low, n.boot)

		# True slopes
		slope.low <- true.boot.lm.low$slope
		slope.sd.low <- true.boot.lm.low$slope.sd
		slope.high <- true.boot.lm.high$slope
		slope.sd.high <- true.boot.lm.high$slope.sd

		# True intercepts
		inter.low <- true.boot.lm.low$inter
		inter.sd.low <- true.boot.lm.low$inter.sd
		inter.high <- true.boot.lm.high$inter
		inter.sd.high <- true.boot.lm.high$inter.sd

		# True R Squared coefficients
		r.sqr.low <- true.boot.lm.low$r2
		r.sqr.high <- true.boot.lm.high$r2
	}

	# True statistics
	slope.alt.stat.true <- abs(slope.high - slope.low)
	inter.alt.stat.true <- abs(inter.high - inter.low)
	slope.stat.true <- slope.alt.stat.true/sqrt(slope.sd.high^2 + slope.sd.low^2)
	inter.stat.true <- inter.alt.stat.true/sqrt(inter.sd.high^2 + inter.sd.low^2)
	total.stat.true <- slope.stat.true + inter.stat.true
	r.sqr.stat.true <- abs(r.sqr.high - r.sqr.low)

	# Construct data
	data.high <-cbind(col.x.high, col.y.high)
	data.low <-cbind(col.x.low, col.y.low)
	data.all <- as_tibble(rbind(data.high, data.low))

	n.low <- nrow(data.low)
	n.high <- nrow(data.high)
	n.all <- n.low + n.high

	# Prepare variables for the test
	# n.sim <- 5000  # Number of simulations
	slope.alt.stat.sim <- numeric(n.sim)
	inter.alt.stat.sim <- numeric(n.sim)
	slope.stat.sim <- numeric(n.sim)
	inter.stat.sim <- numeric(n.sim)
	total.stat.sim <- numeric(n.sim)
	r.sqr.stat.sim <- numeric(n.sim)

	# Prepare counters
	slope.alt.count <- 0
	inter.alt.count <- 0
	slope.count <- 0
	inter.count <- 0
	total.count <- 0
	r.sqr.count <- 0

	# Perform the permutation test
	for (i in 1:n.sim){
		data.sample <- sample_n(data.all, n.all)

		low <- cbind(data.sample[1:n.low, 1], data.sample[1:n.low, 2])
		x <- n.low + 1
		high <- cbind(data.sample[x:n.all, 1], data.sample[x:n.all, 2])

		if (!bootstrap) {
			# Fit the new data
			fit.low.perm <- lm(log10(low[,2]) ~ log10(low[,1]))
			sum.fit.low.perm <- summary(fit.low.perm)
			fit.high.perm <- lm(log10(high[,2]) ~ log10(high[,1]))
			sum.fit.high.perm <- summary(fit.high.perm)

			# Simulated slopes
			slope.low.perm <- sum.fit.low.perm$coefficients[2]
			slope.sd.low.perm <- sum.fit.low.perm$coefficients[4]
			slope.high.perm <- sum.fit.high.perm$coefficients[2]
			slope.sd.high.perm <- sum.fit.high.perm$coefficients[4]

			# Simulated intercepts
			inter.low.perm <- sum.fit.low.perm$coefficients[1]
			inter.sd.low.perm <- sum.fit.low.perm$coefficients[3]
			inter.high.perm <- sum.fit.high.perm$coefficients[1]
			inter.sd.high.perm <- sum.fit.high.perm$coefficients[3]

			# Simulated R Squared coefficients
			r.sqr.low.perm <- sum.fit.low.perm$r.squared
			r.sqr.high.perm <- sum.fit.high.perm$r.squared
		} else if (bootstrap) {
			# Simulated bootstrap statistics
			# sim.boot.lm.high <- do_bootstrap(high[,1], high[,2], n.boot)
			# sim.boot.lm.low <- do_bootstrap(low[,1], low[,2], n.boot)

			sim.boot.lm.high <- perform_bootstrap(high[,1], high[,2], n.boot)
			sim.boot.lm.low <- perform_bootstrap(low[,1], low[,2], n.boot)

			# Simulated slopes
			slope.low.perm <- sim.boot.lm.low$slope
			slope.sd.low.perm <- sim.boot.lm.low$slope.sd
			slope.high.perm <- sim.boot.lm.high$slope
			slope.sd.high.perm <- sim.boot.lm.high$slope.sd

			# Simulated intercepts
			inter.low.perm <- sim.boot.lm.low$inter
			inter.sd.low.perm <- sim.boot.lm.low$inter.sd
			inter.high.perm <- sim.boot.lm.high$inter
			inter.sd.high.perm <- sim.boot.lm.high$inter.sd

			# Simulated R Squared coefficients
			r.sqr.low.perm <- sim.boot.lm.low$r2
			r.sqr.high.perm <- sim.boot.lm.high$r2
		}

		# Simulated statistics
		slope.alt.stat.sim[i] <- abs(slope.high.perm - slope.low.perm)
		inter.alt.stat.sim[i] <- abs(inter.high.perm - inter.low.perm)
		slope.stat.sim[i] <- slope.alt.stat.sim[i]/sqrt(slope.sd.high.perm^2 + slope.sd.low.perm^2)
		inter.stat.sim[i] <- inter.alt.stat.sim[i]/sqrt(inter.sd.high.perm^2 + inter.sd.low.perm^2)
		total.stat.sim[i] <- slope.stat.sim[i] + inter.stat.sim[i]
		r.sqr.stat.sim[i] <- abs(r.sqr.high.perm - r.sqr.low.perm)

		# Counters for p-values
		if (slope.alt.stat.sim[i] > slope.alt.stat.true) {
			slope.alt.count <- slope.alt.count + 1
		}
		if (inter.alt.stat.sim[i] > inter.alt.stat.true) {
			inter.alt.count <- inter.alt.count + 1
		}
		if (slope.stat.sim[i] > slope.stat.true) {
			slope.count <- slope.count + 1
		}
		if (inter.stat.sim[i] > inter.stat.true) {
			inter.count <- inter.count + 1
		}
		if (total.stat.sim[i] > total.stat.true) {
			total.count <- total.count + 1
		}
		if (r.sqr.stat.sim[i] < r.sqr.stat.true) {
			r.sqr.count <- r.sqr.count + 1
		}
	}

	get_pval_error <- function(p) {
		return(1.96 * (sqrt((p - (p)^2) / n.sim)))
	}

	# Calculate p-value and its error
	slope.alt.p.value <- slope.alt.count/n.sim
	slope.alt.p.value.err <- get_pval_error(slope.alt.p.value)

	inter.alt.p.value <- inter.alt.count/n.sim
	inter.alt.p.value.err <- get_pval_error(inter.alt.p.value)

	slope.p.value <- slope.count/n.sim
	slope.p.value.err <- get_pval_error(slope.p.value)

	inter.p.value <- inter.count/n.sim
	inter.p.value.err <- get_pval_error(inter.p.value)

	total.p.value <- total.count/n.sim
	total.p.value.err <- get_pval_error(total.p.value)

	r.sqr.p.value <- r.sqr.count/n.sim
	r.sqr.p.value.err <- get_pval_error(r.sqr.p.value)

	# Format results
	results.df <- tibble(
		cbind(
			# Slopes (Alt)
			slope.alt.p.val = slope.alt.p.value,
			slope.alt.p.val.error = slope.alt.p.value.err,
			# Intercepts (Alt)
			inter.alt.p.val = inter.alt.p.value,
			inter.alt.p.val.error = inter.alt.p.value.err,
			# Slopes
			slope.p.val = slope.p.value,
			slope.p.val.error = slope.p.value.err,
			# Intercepts
			inter.p.val = inter.p.value,
			inter.p.val.error = inter.p.value.err,
			# Total
			total.p.val = total.p.value,
			total.p.val.error = total.p.value.err,
			# R Squared
			r.sqr.p.val = r.sqr.p.value,
			r.sqr.p.val.error = r.sqr.p.value.err
		)
	)

	return(results.df)
}


# Summarise p-values ---------------------------------------

summarise_p_values <- function(basin, var1, var2, min.speed = 0,
															 bootstrap = F, n.sim = 1000) {
	basin.df <- eval(parse(text=paste("pdi.", tolower(basin), sep = "")))

	n.boot <- n.sim / 2

	# var2 ~ var1 regression (y ~ x)
	p.val.yx <- do_permutation_test(basin.df, var2, var1, min.speed, bootstrap, n.sim, n.boot)

	# var1 ~ var2 regression (x ~ y)
	p.val.xy <- do_permutation_test(basin.df, var1, var2, min.speed, bootstrap, n.sim, n.boot)


	# Construct summarised data frame
	results <- tibble(
		rbind(
			cbind(p.val.yx, dep.var = var2, indep.var = var1,
						basin = toupper(basin), min.speed = min.speed),
			cbind(p.val.xy, dep.var = var1, indep.var = var2,
						basin = toupper(basin), min.speed = min.speed)
		)
	)

	i <- sapply(results, is.factor)
	results[i] <- lapply(results[i], as.character)

	return(results)
}
