# Code to analyse the residuals in the linear model using statistical hypothesis testing
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

library(lmtest)
library(nortest)
library(tidyverse)


# Functions ------------------------------------------------

true_gqtest <- function(col.x, col.y, n.sim = 100) {
	data <- data.frame(col.x, col.y)
	GQ.true <- gqtest(lm(data[[2]] ~ data[[1]]), fraction = 1/3)$statistic

	GQ.sim <- c()
	count <- 0
	for (i in 1:n.sim) {
		data.sim <- dplyr::sample_n(data, floor(nrow(data)/2), replace = F)
		GQ.sim[i] <- gqtest(lm(data.sim[[2]] ~ data.sim[[1]]), fraction = 1/3)$statistic

		if (GQ.sim[i] > GQ.true) {
			count <- count + 1
		}
	}

	return(count / n.sim)
}

true_lillietest <- function(col.x, col.y, n.sim = 100) {
	data <- data.frame(col.x, col.y)
	D.true <- lillie.test(lm(data[[2]] ~ data[[1]])$residuals)$statistic

	D.sim <- c()
	count <- 0
	for (i in 1:n.sim) {
		data.sim <- dplyr::sample_n(data, nrow(2), replace = T)
		# print(data.sim)
		D.sim[i] <- lillie.test(lm(data.sim[[2]] ~ data.sim[[1]])$residuals)$statistic

		if (D.sim[i] > D.true) {
			count <- count + 1
		}
	}

	return(count / n.sim)
	# return(D.sim)
}



# Tests ----------------------------------------------------

set.seed(10)
resid.data <- dplyr::tibble(
	x = runif(75, 10, 150)
)

set.seed(10)
resid.data$resid <- rnorm(75, 0, 0.5)
set.seed(94)
resid.data$resid.b <- rnorm(75, 0, 0.5)^2 + runif(75, 0 ,1) ^2
set.seed(12)
resid.data$resid.c <- rnorm(75, 0, 0.5)

resid.data <- resid.data %>%
	dplyr::mutate(
		y = 3 * x + resid,
		y.b = 3 * x + resid.b,
		y.c = 3 * x + (1 + x) * resid.c
	)

gqtest(lm(resid.data$y ~ resid.data$x))
true_gqtest(resid.data$x, resid.data$y, 1000)

gqtest(lm(resid.data$y.b ~ resid.data$x))
true_gqtest(resid.data$x, resid.data$y.b, 1000)

gqtest(lm(resid.data$y.c ~ resid.data$x))
true_gqtest(resid.data$x, resid.data$y.c, 1000)


lillie.test(lm(resid.data$y.c ~ resid.data$x)$residuals)
true_lillietest(resid.data$x, resid.data$y.c, 1000)

lillie.test(lm(resid.data$y ~ resid.data$x)$residuals)
true_lillietest(resid.data$x, resid.data$y, 1000)
