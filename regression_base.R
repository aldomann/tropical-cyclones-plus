# Base code to perform different statistical tests to analise the slopes of PDI vs duration
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Libraries ------------------------------------------------
library(tidyverse)
library(lubridate)

# Explore p-values -----------------------------------------

explore_p_values <- function(p.values.list, alpha = 0.05) {
	for (i in 1:length(p.values.list)) {
		for (j in 1:2) {
			if ( (p.values.list[[i]][["slope.p.val"]][j] <= alpha) |
					 (p.values.list[[i]][["inter.p.val"]][j] <= alpha) |
					 (p.values.list[[i]][["total.p.val"]][j] <= alpha) |
					 (p.values.list[[i]][["r.sqr.p.val"]][j] <= alpha) ) {
				# Print regression results
				print(paste("Element in list:", i))
				print(p.values.list[[i]][j, ])
			}
		}
	}
}

# Compare statistics and CI methods ------------------------

compare_perm_statistics <- function(p.values.list) {
	dim <- 2*length(p.values.list)
	slope.factor <- numeric(dim)
	inter.factor <- numeric(dim)
	k <- 1

	for (i in 1:length(p.values.list)) {
		for (j in 1:2) {
			slope.factor[k] <- p.values.list[[i]][j, "slope.alt.p.val"] / p.values.list[[i]][j, "slope.p.val"]
			inter.factor[k] <- p.values.list[[i]][j, "inter.alt.p.val"] / p.values.list[[i]][j, "inter.p.val"]
			k <- k + 1
		}
	}

	data <- tibble(
		n = seq(1:dim),
		slope.factor = slope.factor,
		inter.factor = inter.factor
	)

	results <- tibble(
		stat = c("slope", "intercept"),
		mean = c(mean(slope.factor), mean(inter.factor)),
		sd = c(sd(slope.factor) ,sd(inter.factor))
	)

	gg.slope <- ggplot(data) +
		geom_histogram(aes(x = slope.factor), binwidth = 0.01, fill = "white", colour = "black") +
		labs(title = "Slope factor")

	gg.inter <- ggplot(data) +
		geom_histogram(aes(x = inter.factor), binwidth = 0.01, fill = "white", colour = "black") +
		labs(title = "Intercept factor")

	# return(data)
	# return(results)
	return(list(gg.slope, gg.inter))
}

compare_perm_methods <- function(p.values.list, boot.p.values.list, alpha = 1.75, beta =0.7) {
	dim <- 6 * 2 * length(p.values.list)
	count = 0

	comp.vect <- numeric(dim)

	for (i in 1:length(p.values.list)) {
		A <- p.values.list[[i]]
		B <- boot.p.values.list[[i]]

		num.vars <- grep("p.val$", names(A), value = T)
		chr.vars <- A[!grepl("p.val", names(A))]

		S <- A[num.vars] / B[match(A$dep.var, B$dep.var), num.vars]

		for (k in 1:length(S)) {
			for (j in 1:2) {
				comp.vect[count] <- S[j, k]
				count = count + 1
			}
		}

		R <- cbind(S, chr.vars)

		print(R)
		print("=================================================================================")

		for (k in 1:(length(R)-4)) {
			for (j in 1:2) {
				if ( R[j, k] >= alpha | R[j, k] <= beta ) {
					print(paste("Element in list:", i))
					print(paste("row:", j, "col:", k))
					print(paste("Value:", R[j, k]))
				}
			}
		}
	}

	gg <- ggplot(tibble(n = seq(1:length(comp.vect)),
								comp.vect = comp.vect)) +
		geom_histogram(aes(x = comp.vect), binwidth = 0.05, fill = "white", colour = "black") +
		labs(title = "Ordinary / Bootstrap factor")

	return(gg)
}

compare_ci_methods <- function(factors.ci.list) {
	dim <- 4*length(factors.ci.list)
	slope.factor <- numeric(dim)
	slope.sd.factor <- numeric(dim)
	inter.factor <- numeric(dim)
	inter.sd.factor <- numeric(dim)
	r.sqr.factor <- numeric(dim)
	r.sqr.sd.factor <- numeric(dim)
	k <- 1

	for (i in 1:length(factors.ci.list)) {
		for (j in 1:4) {
			slope.factor[k] <- factors.ci.list[[i]][j, "slope"]
			slope.sd.factor[k] <- factors.ci.list[[i]][j, "slope.sd"]
			inter.factor[k] <- factors.ci.list[[i]][j, "inter"]
			inter.sd.factor[k] <- factors.ci.list[[i]][j, "inter.sd"]
			r.sqr.factor[k] <- factors.ci.list[[i]][j, "r2"]
			r.sqr.sd.factor[k] <- factors.ci.list[[i]][j, "r2.sd"]
			k <- k + 1
		}
	}

	data <- tibble(
		n = seq(1:dim),
		slope.factor = slope.factor,
		slope.sd.factor = slope.sd.factor,
		inter.factor = inter.factor,
		inter.sd.factor = inter.sd.factor,
		r.sqr.factor = r.sqr.factor,
		r.sqr.sd.factor = r.sqr.sd.factor
	)

	return(summary(data))
}


# Scatterplots ---------------------------------------------

plot_scatterplot <- function(basin, var1, var2, min.speed = 0) {
	# Parse the basin PDI data frame
	basin.df <- eval(parse(text=paste("pdi.", tolower(basin), sep = "")))

	# Filter data frames
	basin.df <- basin.df %>%
		dplyr::filter(max.wind > min.speed)
	df.low <- basin.df %>%
		dplyr::filter(sst.class == "low")
	df.high <- basin.df %>%
		dplyr::filter(sst.class == "high")

	# Extract data vectors
	col.x.low <- df.low[[var1]]
	col.y.low <- df.low[[var2]]
	col.x.high <- df.high[[var1]]
	col.y.high <- df.high[[var2]]

	lm.high.y <- lm(log10(col.y.high) ~ log10(col.x.high))
	lm.low.y <- lm(log10(col.y.low) ~ log10(col.x.low))
	lm.high.x <- lm(log10(col.x.high) ~ log10(col.y.high))
	lm.low.x <- lm(log10(col.x.low) ~ log10(col.y.low))

	# Automatic plot title
	if (min.speed == 0) {
		storms.str <- "all storms"
	} else {
		storms.str <- paste("wind speed >", min.speed)
	}
	years.str <- paste0( min(basin.df$storm.year), "-", max(basin.df$storm.year))
	title.str <- paste0(basin, " (", storms.str, "; ", years.str, ")" )

	gg <- ggplot() +
		# Scatterplot
		geom_point(data = df.low, aes(x = col.x.low, y = col.y.low, colour = "low"), shape = 5, size = 1) +
		geom_point(data = df.high, aes(x = col.x.high, y = col.y.high, colour = "high"), shape = 1, size = 1) +
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
		labs(title = title.str,
				 x = paste(eval(var1)), y = paste(eval(var2)),
				 colour = "SST class", linetype = "Regression")

	return(gg + theme_bw())
}
