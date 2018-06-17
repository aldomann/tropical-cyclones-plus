# Code to analyse the residuals in the linear model using statistical hypothesis testing
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

library(lmtest)
library(nortest)
library(tidyverse)


# Functions ------------------------------------------------

perform_residual_test <- function(col.x, col.y) {
	data <- data.frame(col.x, col.y)

	fit <- lm(data[[2]] ~ data[[1]])
	norm.p.val <- lillie.test(residuals(fit))$p.value
	inde.p.val <- cor.test(residuals(fit), fitted(fit))$p.value
	homo.p.val <- bptest(fit)$p.value

	return(c(
		"norm.p.val" = norm.p.val,
		"inde.p.val" = inde.p.val,
		"homo.p.val" = homo.p.val
	))
}

# Tests ----------------------------------------------------

## generate a regressor
x <- rep(c(-1,1), 500)
## generate heteroskedastic and homoskedastic disturbances
err1 <- c(rnorm(250, sd=1), rnorm(250, sd=2))
err2 <- rnorm(500)
## generate a linear relationship
y1 <- 1 + x + err1
y2 <- 1 + x + err2
## perform tests
perform_residual_test(x, y1)
perform_residual_test(x, y2)
