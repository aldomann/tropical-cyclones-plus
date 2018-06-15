library(tidyverse)
# library(tidyquant)
source("regression_base.R")
source("residuals_base.R")

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


# Residuals in lm ------------------------------------------
library(quantreg)

set.seed(10)
resid.data <- tibble(
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

gg.models <- ggplot(resid.data) +
	geom_jitter(aes(x = x, y = y.b, colour = "Skewed", shape = "skewed"), height = 20) +
	geom_point(aes(x = x, y = y.c, colour = "Heteroscedastic", shape = "hetero")) +
	geom_jitter(aes(x = x, y = y, colour = "Homoscedastic", shape = "homo"), height = 20) +
	scale_shape_manual(values = c("homo"   = 0,
																"hetero" = 1,
																"skewed" = 2),
										 guide = F) +
	scale_colour_manual(values = c("Homoscedastic"   = "orangered1",
																 "Heteroscedastic" = "forestgreen",
																 "Skewed"          = "purple3"),
											breaks = c("Homoscedastic", "Heteroscedastic", "Skewed")) +
	labs(x = "Predictor X", y = "Response Y", colour = "Model") +
	theme_bw()

gg.models #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "models-example.pdf", width = 5.5, height = 3, dpi = 96, device = cairo_pdf)


plot_normal_qqplot(resid.data[["resid"]], "Sample Quantiles") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "qq-norm-example.pdf", width = 2.5, height = 2.5, dpi = 96, device = cairo_pdf)

plot_normal_qqplot(resid.data[["resid.b"]] , "Sample Quantiles") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "qq-skewed-example.pdf", width = 2.5, height = 2.5, dpi = 96, device = cairo_pdf)

plot_normal_qqplot(resid.data[["resid.c"]] , "Sample Quantiles") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "qq-hetero-example.pdf", width = 2.5, height = 2.5, dpi = 96, device = cairo_pdf)

plot_resid_vs_fitted_from_data(resid.data, "y ~ x") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "resid-norm-example.pdf", width = 2.5, height = 2.5, dpi = 96, device = cairo_pdf)
plot_resid_vs_fitted_from_data(resid.data, "y.b ~ x") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "resid-skewed-example.pdf", width = 2.5, height = 2.5, dpi = 96, device = cairo_pdf)
plot_resid_vs_fitted_from_data(resid.data, "y.c ~ x") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "resid-hetero-example.pdf", width = 2.5, height = 2.5, dpi = 96, device = cairo_pdf)


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
# source("regression_base.R")
# load("slopes_analysis.RData")


# Get RAW data ---------------------------------------------

pdi.all <- as_tibble(data.table::fread('data/hurdat2-hadisst-1966-2016_pdis.csv')) %>%
	mutate(storm.duration = measurements::conv_unit(storm.duration, "sec", "hr"))

pdi.natl <- pdi.all %>%
	dplyr::filter(basin == "NATL")
pdi.epac <- pdi.all %>%
	dplyr::filter(basin == "EPAC")

plot_scatterplot("NATL", "storm.duration", "storm.pdi", 33) + labs(title = "N. Atl. regression analysis (1966-2016)", x = "Storm lifetime (h)", y = bquote(PDI~ (m^3 ~s^-2))) + theme_bw() + theme(text = element_text(family = "Palatino")) + ggsave(filename = "scatter_natl.png", width = 6.5, height = 3., dpi = 300, device = "png")


# PDI ~ duration OLS
get_t_statistics(
	coefs.low =  as.numeric(lm.coefs.list.natl.ds[[4]][5,3:7]),
	coefs.high = as.numeric(lm.coefs.list.natl.ds[[4]][7,3:7])
)

# PDI ~ duration Bootstrap
get_t_statistics(
	coefs.low =  as.numeric(lm.coefs.list.natl.ds[[4]][6,3:7]),
	coefs.high = as.numeric(lm.coefs.list.natl.ds[[4]][8,3:7])
)

# Duration ~ PDI OLS
get_t_statistics(
	coefs.low =  as.numeric(lm.coefs.list.natl.ds[[4]][1,3:7]),
	coefs.high = as.numeric(lm.coefs.list.natl.ds[[4]][3,3:7])
)

# Duration ~ PDI Bootstrap
get_t_statistics(
	coefs.low =  as.numeric(lm.coefs.list.natl.ds[[4]][2,3:7]),
	coefs.high = as.numeric(lm.coefs.list.natl.ds[[4]][4,3:7])
)



