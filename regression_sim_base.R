# Base code simulate bivariate lognormal distributions for hurricane observations
# Author: Alfredo Hernández <aldomann.designs@gmail.com>


# Generic bivariate simulation functions -------------------

estimate_bvnd <- function(df, n.samp) {
	# Mu
	means <- colMeans(df)
	# Sigma
	cov.matrix <- var(df)

	params <- tibble(
		mu.x = means[1],
		mu.y = means[2],
		sigma1 = cov.matrix[1,1],
		sigma2 = cov.matrix[2,2],
		rho = cov.matrix[1,2]
	)

	return(params)
}

simulate_bvnd <- function(df) {
	ests <- estimate_bvnd(df)
	n.samp <- nrow(df)

	means <- c(ests[[1]], ests[[2]])

	cov.matrix <- matrix(c(ests[[3]], ests[[5]],
												 ests[[5]], ests[[4]]),
											 2)

	model <- MASS::mvrnorm(n.samp, mu = means, Sigma = cov.matrix)

	return(as_tibble(model))
}

simulate_bvlnd <- function(df) {
	df <- log10(df)
	ests <- estimate_bvnd(df)
	n.samp <- nrow(df)

	means <- c(ests[[1]], ests[[2]])

	cov.matrix <- matrix(c(ests[[3]], ests[[5]],
												 ests[[5]], ests[[4]]),
											 2)

	model <- MASS::mvrnorm(n.samp, mu = means, Sigma = cov.matrix)

	return(as_tibble(10^model))
}


# Hurricane simulation functions ---------------------------

simulate_hurricane_obs <- function(basin.name, var1, var2, min.speed) {
	# Filter and clean data
	df <- pdi.all %>%
		dplyr::filter(basin == basin.name) %>%
		dplyr::filter(max.wind > min.speed) %>%
		dplyr::select(var1, var2, sst.class)

	df.low <- df %>%
		dplyr::filter(sst.class == "low") %>%
		select(-sst.class)

	df.high <- df %>%
		dplyr::filter(sst.class == "high") %>%
		select(-sst.class)

	# Simulate the data
	sim.df.low <- simulate_bvlnd(df.low) %>%
		mutate(sst.class = "low", max.wind = min.speed + 1) %>%
		rename_(.dots = setNames(c("V1", "V2"),
														 c(var1, var2)))
	sim.df.high <- simulate_bvlnd(df.high) %>%
		mutate(sst.class = "high", max.wind = min.speed + 1) %>%
		rename_(.dots = setNames(c("V1", "V2"),
														 c(var1, var2)))

	return(rbind(sim.df.low, sim.df.high))
}


# Bivariate log normal distributions -----------------------

plot_bvln_dist <- function(pdi.df) {
	gg <- ggplot(pdi.df %>% dplyr::filter(max.wind > 33)) +
		geom_point(aes(y = storm.pdi, x = storm.duration, colour = sst.class, shape = sst.class), size = 1) +
		scale_y_log10() +
		scale_x_log10(breaks = c(25, 50, 100, 200, 400, 800)) +
		scale_shape_manual(values = c("high" = 1, "low" = 5)) +
		scale_colour_manual(values = c("high" = "brown1", "low" = "dodgerblue1")) +
		guides(colour = guide_legend(order = 1, override.aes = list(shape = c(1,5))),
					 shape = FALSE) +
		labs(x = "Storm lifetime (h)", y = bquote(PDI~ (m^3 ~s^-2)),
				 colour = "SST Class") +
		theme_bw()

	return(gg)
}
