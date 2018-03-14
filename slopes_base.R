# Base code to perform different statistical tests to analise the slopes of PDI vs duration
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Libraries ------------------------------------------------
library(tidyverse)
library(lubridate)
library(data.table)

# Confidence interval --------------------------------------

get_conf_interval <- function(df, class, var1, var2, min.speed = 0) {
	# Filter and clean data
	df <- df %>%
		dplyr::filter(max.wind > min.speed) %>%
		dplyr::filter(sst.class == class)
	col1 <- df[,var1]
	col2 <- df[,var2]

	# Construct data
	data <- cbind(log10(col1), log10(col2))
	n <- nrow(data)

	# Linear regression
	fit <- lm(data[,2] ~ data[,1])
	slope <- summary(fit)$coefficients[2]
	slope.sd <- summary(fit)$coefficients[4]
	r.squared <- summary(fit)$r.squared

	# Prepare variables for the simulation
	n.sim <- 1000 # Number of simulations
	data.seq <- seq(1, n)
	slope.sim <- numeric(n.sim)
	slope.sd.sim <- numeric(n.sim)
	r.squared.sim <- numeric(n.sim)
	t.value.sim <- numeric(n.sim)

	# Perform the simulation
	for (i in 1:n.sim) {
		sim.sample <- sample(data.seq, n, replace = T)

		fit.value.sim <- lm(data[sim.sample, 2] ~ data[sim.sample, 1])
		slope.sim[i] <- summary(fit.value.sim)$coefficients[2]
		slope.sd.sim[i] <- summary(fit.value.sim)$coefficients[4]
		r.squared.sim[i] <- summary(fit.value.sim)$r.squared

		t.value.sim[i] <- (slope.sim[i] - slope) / slope.sd.sim[i]
	}

	# The bootstrap-t 95% method
	boot.low <- slope + slope.sd * quantile(t.value.sim, 0.025)
	boot.upp <- slope + slope.sd * quantile(t.value.sim, 0.975)
	boot.sd <- (boot.upp - boot.low) / 2
	boot.slope <- boot.upp - boot.sd
	boot.r.squared <- mean(r.squared.sim)

	# # Simple Method
	# simp.low <- 2 * slope - quantile(slope.sim, 0.975)
	# simp.upp <- 2 * slope - quantile(slope.sim, 0.025)
	# simp.sd <- (simp.upp - simp.low) / 2
	# simp.slope <- simp.upp - simp.sd
	#
	# # Quantile method
	# quan.low <- quantile(slope.sim, 0.025)
	# quan.upp <- quantile(slope.sim, 0.975)
	# quan.sd <- (quan.upp - quan.low) / 2
	# quan.slope <- quan.upp - quan.sd

	# results.df <- data.frame(
	# 	method = c("lm", "bootstrap-t", "simple", "quantile"),
	# 	slope = c(slope, boot.slope, simp.slope, quan.slope),
	# 	sd = c(slope.sd, boot.sd, simp.sd, quan.sd)
	# 	)

	results.df <- data.frame(
		method = c("lm", "bootstrap-t"),
		slope = c(slope, boot.slope),
		sd = c(slope.sd, boot.sd),
		r2 = c(r.squared, boot.r.squared),
		dep.var = rep(var2, 2),
		indep.var = rep(var1, 2),
		row.names = 1
	)

	i <- sapply(results.df, is.factor)
	results.df[i] <- lapply(results.df[i], as.character)

	return(results.df)
}

summarise_conf_intervals <- function(basin, var1, var2, min.speed = 0) {
	# Parse the basin PDI data frame
	basin.df <- eval(parse(text=paste("pdi.", tolower(basin), sep = "")))

	# var2 ~ var1 regression (y ~ x)
	ci.yx.low <-  get_conf_interval(basin.df, "low",  var1, var2, min.speed)
	ci.yx.high <- get_conf_interval(basin.df, "high", var1, var2, min.speed)

	# var1 ~ var2 regression (x ~ y)
	ci.xy.low <-  get_conf_interval(basin.df, "low",  var2, var1, min.speed)
	ci.xy.high <- get_conf_interval(basin.df, "high", var2, var1, min.speed)

	# Construct summarised data frame
	results <- data.frame(
		rbind(
			cbind(ci.yx.low["bootstrap-t",]),
			cbind(ci.yx.high["bootstrap-t",]),
			cbind(ci.xy.low["bootstrap-t",]),
			cbind(ci.xy.high["bootstrap-t",])
		)
	)
	rownames(results) <- c()

	return(results)
}


# Permutation test -----------------------------------------

do_permutation_test <- function(df, var1, var2, min.speed = 0) {
	# Filter max wind speed
	df <- df %>%
		dplyr::filter(max.wind > min.speed)

	# Filter and clean data (low SST)
	df.low <- df %>%
		dplyr::filter(sst.class == "low")
	col1.low <- df.low[,var1]
	col2.low <- df.low[,var2]

	# Filter and clean data (high SST)
	df.high <- df %>%
		dplyr::filter(sst.class == "high")
	col1.high <- df.high[,var1]
	col2.high <- df.high[,var2]

	# Linear regressions statistics
	fit.low <- lm(log10(col2.low) ~ log10(col1.low))
	fit.high <- lm(log10(col2.high) ~ log10(col1.high))

	slope.low <- summary(fit.low)$coefficients[2]
	sd.low <- summary(fit.low)$coefficients[4]
	slope.high <- summary(fit.high)$coefficients[2]
	sd.high <- summary(fit.high)$coefficients[4]

	stat.true <- abs(slope.high - slope.low)/sqrt(sd.high^2 + sd.low^2)

	# Construct data
	data.high <-cbind(log10(col1.high), log10(col2.high))
	data.low <-cbind(log10(col1.low), log10(col2.low))
	data.all <- rbind(data.high, data.low)

	n.low <- nrow(data.low)
	n.high <- nrow(data.high)
	n.all <- n.low + n.high

	# Prepare variables for the test
	n.sim <- 5000  # Number of simulations
	stat.sim <- numeric(n.sim)
	count <- 0

	# Perform the permutation test
	for (i in 1:n.sim){
		col1.sample <- sample (data.all[,1], n.all)
		col2.sample <- sample (data.all[,2], n.all)
		data.sample <- cbind(col1.sample, col2.sample)

		low <- cbind(data.sample[1:n.low, 1], data.sample[1:n.low, 2])
		x <- n.low + 1
		high <- cbind(data.sample[x:n.all, 1], data.sample[x:n.all, 2])

		fit.lowp <- lm(log10(low[,2]) ~ log10(low[,1]))
		fit.highp <- lm(log10(high[,2]) ~ log10(high[,1]))
		slope.lowp <- summary(fit.lowp)$coefficients[2]
		sd.lowp <- summary(fit.lowp)$coefficients[4]
		slope.highp <- summary(fit.highp)$coefficients[2]
		sd.highp <- summary(fit.highp)$coefficients[4]
		stat.sim[i] <- abs(slope.highp - slope.lowp)/sqrt(sd.highp^2 + sd.lowp^2)

		if (stat.sim[i] > stat.true) {
			count <- count + 1
		}
	}
	# Calculate p-value and its error
	p.value <- count/n.sim
	p.value.err <- 1.96 * (sqrt((p.value - (p.value)^2) / n.sim))

	return( c(p.value, p.value.err) )
}

# Scatterplots ---------------------------------------------

plot_scatterplot <- function(df, var1, var2, min.speed = 0) {
	# Filter data frames
	df <- df %>%
		dplyr::filter(max.wind > min.speed)
	df.low <- df %>%
		dplyr::filter(sst.class == "low")
	df.high <- df %>%
		dplyr::filter(sst.class == "high")

	# Extract data vectors
	col1.low <- df.low[,var1]
	col2.low <- df.low[,var2]
	col1.high <- df.high[,var1]
	col2.high <- df.high[,var2]

	lm.high.y <- lm(log10(col2.high) ~ log10(col1.high))
	lm.low.y <- lm(log10(col2.low) ~ log10(col1.low))
	lm.high.x <- lm(log10(col1.high) ~ log10(col2.high))
	lm.low.x <- lm(log10(col1.low) ~ log10(col2.low))

	# years.str <- paste0(year(ssts$year[1]), "-", year(ssts$year[length(ssts$year)]))

	gg <- ggplot() +
		# Scatterplot
		geom_point(data = df.low, aes(x = col1.low, y = col2.low, colour = "low"), shape = 5, size = 1) +
		geom_point(data = df.high, aes(x = col1.high, y = col2.high, colour = "high"), shape = 1, size = 1) +
		# Regression lines
		geom_abline(aes(slope = coef(lm.low.y)[[2]],
										intercept = coef(lm.low.y)[[1]],
										colour = "low", linetype = "y(x)")) +
		geom_abline(aes(slope = 1/coef(lm.low.x)[[2]],
										intercept = -coef(lm.low.x)[[1]]/coef(lm.low.x)[[2]],
										colour = "low", linetype = "x(y)")) +
		geom_abline(aes(slope = coef(lm.high.y)[[2]],
										intercept = coef(lm.high.y)[[1]],
										colour = "high", linetype = "y(x)")) +
		geom_abline(aes(slope = 1/coef(lm.high.x)[[2]],
										intercept = -coef(lm.high.x)[[1]]/coef(lm.high.x)[[2]],
										colour = "high", linetype = "x(y)")) +
		# Scales and legend
		scale_x_log10(breaks = c(25, 50, 100, 200, 400, 800)) +
		scale_y_log10() +
		guides(colour = guide_legend(order = 1, override.aes = list(linetype = c(0,0), shape = c(1,5))),
					 linetype = guide_legend(order = 2, override.aes = list(colour = c("black", "black")))) +
		scale_colour_manual(labels = c(bquote(.(paste0("high; ")) ~ r^2 ~
																						.(paste0("= ", format(summary(lm.high.y)$r.squared, digits = 2)))),
																	 bquote(.(paste0("low;  ")) ~ r^2 ~
																	 			 	.(paste0("= ", format(summary(lm.low.y)$r.squared, digits = 2))))),
												values = c("high" = "brown1", "low" = "dodgerblue1")) +
		scale_linetype_manual(values = c("x(y)" = "longdash", "y(x)" = "solid")) +
		labs(
			#title = paste0("PDI vs duration scatterplot", " (", attr(ssts, "title"), "; ", years.str ,")") ,
				 x = "Storm duration (h)", y = bquote(PDI~ (m^3 ~s^-2)),
				 colour = "SST class", linetype = "Regression")

	return(gg)
}
