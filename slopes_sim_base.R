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
