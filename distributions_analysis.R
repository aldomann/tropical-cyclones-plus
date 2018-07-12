# Code study the population distributions for hurricane observations
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Source base code -----------------------------------------
source("regression_base.R")
source("distributions_base.R")

# Get RAW data ---------------------------------------------

pdi.all <- as_tibble(data.table::fread('data/hurdat2-hadisst-1966-2016_pdis.csv')) %>%
	mutate(storm.duration = measurements::conv_unit(storm.duration, "sec", "hr"))

pdi.natl <- pdi.all %>%
	dplyr::filter(basin == "NATL")
pdi.epac <- pdi.all %>%
	dplyr::filter(basin == "EPAC")


# Simulate BVLN distributions ------------------------------

# pdi.natl.sim <- simulate_hurricane_obs("NATL", "storm.pdi", "storm.duration", 0)
# pdi.epac.sim <- simulate_hurricane_obs("EPAC", "storm.pdi", "storm.duration", 0)

# Permutation tests (OLS)
# n.sim.test <- 1000
# true.p.vals.natl.pdi.ds.std <- readRDS("objects/slopes_p_values_pdi.rds")[[4]]
# p.vals.natl.pdi.ds.std <- summarise_p_values("NATL.sim", "storm.duration", "storm.pdi", 33, F, n.sim.test)

# Permutation tests (bootstrap)
# true.p.vals.natl.pdi.ds.boot <- readRDS("objects/slopes_p_values_boot_pdi.rds")[[4]]
# p.vals.natl.pdi.ds.boot <- summarise_p_values("NATL.sim", "storm.duration", "storm.pdi", 33, T, n.sim.test)


# Plot real BVLN distributions ------------------------------

plot_bvln_dist(pdi.natl) #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl-bvln.pdf", width = 5, height = 3.5, dpi = 96, device = cairo_pdf)
plot_bvln_dist(pdi.epac) #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac-bvln.pdf", width = 5, height = 3.5, dpi = 96, device = cairo_pdf)


# Marginals analsysis --------------------------------------

natl.data <- pdi.natl %>%
	dplyr::filter(max.wind > 33) %>%
	select(storm.pdi, storm.duration, max.wind, sst.class)

epac.data <- pdi.epac %>%
	dplyr::filter(max.wind > 33) %>%
	select(storm.pdi, storm.duration, max.wind, sst.class)

# Plot the marginals and the mean
plot_marginal(natl.data, "storm.duration", "lifetime") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl-marginals-lifetime.pdf", width = 4, height = 2.5, dpi = 96, device = cairo_pdf)
plot_marginal(natl.data, "storm.pdi", "PDI") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl-marginals-pdi.pdf", width = 4, height = 2.5, dpi = 96, device = cairo_pdf)

plot_marginal(epac.data, "storm.duration", "lifetime") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac-marginals-lifetime.pdf", width = 4, height = 2.5, dpi = 96, device = cairo_pdf)
plot_marginal(epac.data, "storm.pdi", "PDI") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac-marginals-pdi.pdf", width = 4, height = 2.5, dpi = 96, device = cairo_pdf)


summarise_marginals_stats("NATL", 33)
summarise_marginals_stats("EPAC", 33)

summarise_marginals_stats_nolog("NATL", 33) %>% data.frame()
summarise_marginals_stats_nolog("EPAC", 33)
