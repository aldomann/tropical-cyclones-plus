library(tidyverse)

# extrafont::font_import(paths = "/home/aldomann/Design/Typefaces/2. Sans Serif/1. Humanist/Raleway")
# extrafont::font_import(paths = "/home/aldomann/Downloads/newpx/ttf")


# Linear regression ----------------------------------------

set.seed(11)

X <- rnorm(20, 50, 5)
epsilon <- rnorm(20, 0, 5)
Y <- 3 * X + epsilon


plot(Y ~ X)

data <- tibble(X, Y)

gg <- ggplot(data) +
	aes(x = X, y = Y) +
	geom_point(shape = 1, size = 2) +
	geom_smooth(method = "lm", level = 0.95, colour = "black") +
	labs(x = "Predictor X", y = "Response Y") +
	theme_bw()

gg + theme(text = element_text(family = "Palatino")) #+ ggsave(filename = "ci-example.pdf", width = 5.5, height = 3, dpi = 96, device = cairo_pdf)

summary(lm(data = data, Y ~ X))

summary(data)


resid.data <- tibble(
	x = runif(75, 10, 15)
)
set.seed(19)
resid.data$resid <- rnorm(75, 0, 0.5)
set.seed(5)
resid.data$resid.b <- rnorm(75, 0, 0.5)^2 + runif(75, 0 ,1) ^2

resid.data <- resid.data %>%
	dplyr::mutate(
		y = 3 * x + resid,
		y.b = 3 * x + resid.b
		)

plot_qqplot(resid.data[["resid"]], "Sample Quantiles") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "qq-norm-example.pdf", width = 3, height = 3, dpi = 96, device = cairo_pdf)

plot_qqplot(resid.data[["resid.b"]] , "Sample Quantiles") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "qq-bad-example.pdf", width = 3, height = 3, dpi = 96, device = cairo_pdf)


plot_resid_vs_fitted_from_data <- function(data, formula) {

	fit <- lm(data = data, eval(formula))
	fit.data <- tibble(
		fitted = fitted(fit),
		resid = resid(fit)
		# resid = data[["resid"]]
		)

	gg <- ggplot(fit.data) +
		aes(x = fitted, y = resid) +
		geom_point(shape = 1, size = 2.5) +
		geom_smooth(se = F, method = "auto", colour = "red") +
		geom_hline(yintercept = 0, linetype = "dashed") +
		labs(x = "Fitted values", y = "Residuals") +
		theme_bw()

	return(gg)
}

plot_resid_vs_fitted_from_data(resid.data, "y ~ x") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "resid-norm-example.pdf", width = 3, height = 3, dpi = 96, device = cairo_pdf)
plot_resid_vs_fitted_from_data(resid.data, "y.b ~ x") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "resid-bad-example.pdf", width = 3, height = 3, dpi = 96, device = cairo_pdf)


# Poster ---------------------------------------------------
source("geographic_base.R")
library(measurements)

# Get RAW data ---------------------------------------------

storms.all <- as_tibble(fread('data/hurdat2-all.csv'))

storms.natl <- storms.all %>%
	dplyr::filter(basin == "NATL") %>%
	dplyr::filter(storm.year >= 1966)

storms.epac <- storms.all %>%
	dplyr::filter(basin == "EPAC") %>%
	dplyr::filter(storm.year >= 1986)

storms.natl <- arrange(storms.natl, date.time)
storms.epac <- arrange(storms.epac, date.time)

storms.all <- rbind(storms.natl, storms.epac)

storms.tracks <- storms.all %>%
	dplyr::filter(n.obs > 1) %>%
	group_by(storm.id) %>%
	# mutate(distance = distance_haversine(lat, lag(lat), long, lag(long))) %>%
	mutate(distance = distance_slc(lat, lag(lat), long, lag(long))) %>%
	mutate(distance = ifelse(is.na(distance), 0, distance)) %>%
	summarise(first.lat = first(lat), last.lat = last(lat),
						first.long = first(long), last.long = last(long),
						distance = sum(distance))

pdi.all <- as_tibble(fread('data/hurdat2-hadisst-1966-2016_pdis.csv')) %>%
	mutate(storm.duration = conv_unit(storm.duration, "sec", "hr"))

# Join data frames by storm.id
storms.joint <- full_join(pdi.all, storms.tracks)

storms.joint <- storms.joint %>%
	dplyr::filter(!is.na(storm.name))

# devtools::install_github("rplzzz/ggalt", ref = "ggp221", dependencies = T)

# Windows of activity
# years.natl <- 1966:2016
coords.natl <- c("90W", "20W", "5N", "25N")
coords.natl.map <- c("100W", "0E", "0N", "60N")

# years.epac <- 1966:2016
coords.epac <- c("120W", "90W", "5N", "20N")
coords.epac.map <- c("160W", "90W", "5N", "35N")

coords.all.map <- c("160W", "0E", "0N", "60N")

map_region_hurrs(storms.natl, coords.natl.map, coords.natl, steps = c(20, 10), xtra.lims = c(3,2)) + theme_bw() + theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl_map.png", width = 6, height = 3, dpi = 300, device = "png")


map_region_hurrs_full(storms.all, coords.all.map, coords.natl, coords.epac, steps = c(20, 10), xtra.lims = c(3,2)) + theme_bw() + theme(text = element_text(family = "Palatino")) + ggsave(filename = "full_map.png", width = 6, height = 2.75, dpi = 300, device = "png")



# Source base code -----------------------------------------
source("slopes_base.R")
# load("slopes_analysis.RData")


# Get RAW data ---------------------------------------------

pdi.all <- as_tibble(data.table::fread('data/hurdat2-hadisst-1966-2016_pdis.csv')) %>%
	mutate(storm.duration = measurements::conv_unit(storm.duration, "sec", "hr"))

pdi.natl <- pdi.all %>%
	dplyr::filter(basin == "NATL")
pdi.epac <- pdi.all %>%
	dplyr::filter(basin == "EPAC")

plot_scatterplot("NATL", "storm.duration", "storm.pdi", 33) + labs(title = "N. Atl. regression analysis (1966-2016)", x = "Storm lifetime (h)", y = bquote(PDI~ (m^3 ~s^-2))) + theme_bw() + theme(text = element_text(family = "Palatino")) + ggsave(filename = "scatter_natl.png", width = 6.5, height = 3., dpi = 300, device = "png")



get_T_statistics <- function(a, b, ci) {
	T1 <- abs( ci[[a, 4]] - ci[[b, 4]])
	T2 <- abs( ci[[a, 2]] - ci[[b, 2]])
	T3 <- abs( ci[[a, 6]] - ci[[b, 6]])
	T4 <- abs( ci[[a, 4]] - ci[[b, 4]]) / sqrt(ci[[a, 5]]^2 + ci[[b, 5]]^2 )
	T5 <- abs( ci[[a, 2]] - ci[[b, 2]]) / sqrt(ci[[a, 3]]^2 + ci[[b, 3]]^2 )
	T6 <- T5 + T4

	result <- c(T1,T2,T3,T4,T5,T6)

	return(round(result, digits = 3))
}

get_T_statistics(1, 4,ci.natl.pdi.ds) # normal
get_T_statistics(7, 10,ci.natl.pdi.ds)

get_T_statistics(2, 5,ci.natl.pdi.ds) # boot
get_T_statistics(8, 11,ci.natl.pdi.ds)
