# Code simulate bivariate lognormal distributions for hurricane observations
# Author: Alfredo Hernández <aldomann.designs@gmail.com>


# Source base code -----------------------------------------
source("regression_base.R")
source("regression_sim_base.R")

# Get RAW data ---------------------------------------------

pdi.all <- as_tibble(data.table::fread('data/hurdat2-hadisst-1966-2016_pdis.csv')) %>%
	mutate(storm.duration = measurements::conv_unit(storm.duration, "sec", "hr"))

pdi.natl <- pdi.all %>%
	dplyr::filter(basin == "NATL")
pdi.epac <- pdi.all %>%
	dplyr::filter(basin == "EPAC")

# Simulate data from data ----------------------------------

pdi.natl.small <- pdi.natl %>%
	dplyr::filter(sst.class == "high") %>%
	dplyr::select(storm.pdi, storm.duration)

sim.pdi.natl <- simulate_bvlnd(pdi.natl.small)

# Testing the simulation
ggplot(sim.pdi.natl) +
	geom_point(aes(x = V1, y = V2), colour = "red") +
	geom_point(data = pdi.natl,
						 aes(x = storm.pdi, y = storm.duration), colour = "blue") +
	scale_x_log10() +
	scale_y_log10()


# NATL simulation ------------------------------------------

pdi.natl.sim <- simulate_hurricane_obs("NATL", "storm.pdi", "storm.duration", 33)

gg.sim <- ggplot(pdi.natl.sim) +
	geom_point(aes(y = storm.pdi, x = storm.duration, colour = sst.class)) +
	scale_y_log10(limits = c(1e8, 1e12)) +
	scale_x_log10(limits = c(25, 1000)) +
	labs(title = "Simulated data (NATL, developing systems)") +
	theme_bw()

#gg.sim #+ ggsave(filename = "sim-data.pdf", width = 5.75, height = 3.75, dpi = 96, device = cairo_pdf)
plot_bvln_dist(pdi.natl) + theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl-bvln.pdf", width = 5, height = 2.25, dpi = 96, device = cairo_pdf)
plot_bvln_dist(pdi.epac) + theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac-bvln.pdf", width = 5, height = 2.25, dpi = 96, device = cairo_pdf)

# Estimate confidence intervals
(ci.natl.pdi.ds <- summarise_conf_intervals("NATL", "storm.duration", "storm.pdi", 33))
(ci.natl.sim.pdi.ds <- summarise_conf_intervals("NATL.sim", "storm.duration", "storm.pdi", 33))

# Permutation tests ----------------------------------------

# Permutation tests (OLS)
n.sim.test <- 1000
(true.p.vals.natl.pdi.ds.std <- readRDS("slopes_p_values_pdi.rds")[[4]])
(p.vals.natl.pdi.ds.std <- summarise_p_values("NATL.sim", "storm.duration", "storm.pdi", 33, F, n.sim.test))

# Permutation tests (bootstrap)
(p.vals.natl.pdi.ds.boot <- summarise_p_values("NATL.sim", "storm.duration", "storm.pdi", 33, T, n.sim.test))
(true.p.vals.natl.pdi.ds.boot <- readRDS("slopes_p_values_boot_pdi.rds")[[4]])
