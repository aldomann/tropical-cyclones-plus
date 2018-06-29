# Code to analyse the residuals in the linear model
# Author: Alfredo Hernández <aldomann.designs@gmail.com>


# Source base code -----------------------------------------
source("regression_base.R")
source("resampling_base.R")
source("residuals_base.R")

# Get RAW data ---------------------------------------------

pdi.all <- as_tibble(data.table::fread('data/hurdat2-hadisst-1966-2016_pdis.csv')) %>%
	mutate(storm.duration = measurements::conv_unit(storm.duration, "sec", "hr"))

pdi.natl <- pdi.all %>%
	dplyr::filter(basin == "NATL")
pdi.epac <- pdi.all %>%
	dplyr::filter(basin == "EPAC")

# Clean data -----------------------------------------------

natl.data.low <- pdi.natl %>%
	dplyr::filter(sst.class == "low") %>%
	dplyr::filter(max.wind > 33)

natl.data.high <- pdi.natl %>%
	dplyr::filter(sst.class == "high") %>%
	dplyr::filter(max.wind > 33)

epac.data.low <- pdi.epac %>%
	dplyr::filter(sst.class == "low") %>%
	dplyr::filter(max.wind > 33)

epac.data.high <- pdi.epac %>%
	dplyr::filter(sst.class == "high") %>%
	dplyr::filter(max.wind > 33)


# Linear models --------------------------------------------

natl.fit.low <- lm(data = natl.data.low, log10(storm.pdi) ~ log10(storm.duration))
natl.fit.high <- lm(data = natl.data.high, log10(storm.pdi) ~ log10(storm.duration))

epac.fit.low <- lm(data = epac.data.low, log10(storm.pdi) ~ log10(storm.duration))
epac.fit.high <- lm(data = epac.data.high, log10(storm.pdi) ~ log10(storm.duration))

set.seed(1)
natl.boot.low <- perform_bootstrap(natl.data.low$storm.duration, natl.data.low$storm.pdi, 5000)
natl.boot.low.stats <- natl.boot.low[[1]]
natl.boot.low.data <- natl.boot.low[[2]]

set.seed(1)
natl.boot.high <- perform_bootstrap(natl.data.high$storm.duration, natl.data.high$storm.pdi, 5000)
natl.boot.high.stats <- natl.boot.high[[1]]
natl.boot.high.data <- natl.boot.high[[2]]

set.seed(1)
epac.boot.low <- perform_bootstrap(epac.data.low$storm.duration, epac.data.low$storm.pdi, 5000)
epac.boot.low.stats <- epac.boot.low[[1]]
epac.boot.low.data <- epac.boot.low[[2]]

set.seed(1)
epac.boot.high <- perform_bootstrap(epac.data.high$storm.duration, epac.data.high$storm.pdi, 5000)
epac.boot.high.stats <- epac.boot.high[[1]]
epac.boot.high.data <- epac.boot.high[[2]]


# Residuals vs fitted values -------------------------------
plot_resid_vs_fitted(natl.fit.low) #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl_low_resid_fitted.pdf", width = 4, height = 2.5, dpi = 300, device = cairo_pdf)
plot_resid_vs_fitted(natl.fit.high) #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl_high_resid_fitted.pdf", width = 4, height = 2.5, dpi = 300, device = cairo_pdf)

plot_resid_vs_fitted(epac.fit.low) #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac_low_resid_fitted.pdf", width = 4, height = 2.5, dpi = 300, device = cairo_pdf)
plot_resid_vs_fitted(epac.fit.high) #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac_high_resid_fitted.pdf", width = 4, height = 2.5, dpi = 300, device = cairo_pdf)


# QQ Plots (residuals) -------------------------------------
plot_resid_qqplot(natl.fit.low) #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl_low_resid_qqplot.pdf", width = 4, height = 2.5, dpi = 300, device = cairo_pdf)
plot_resid_qqplot(natl.fit.high) #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl_high_resid_qqplot.pdf", width = 4, height = 2.5, dpi = 300, device = cairo_pdf)

plot_resid_qqplot(epac.fit.low) #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac_low_resid_qqplot.pdf", width = 4, height = 2.5, dpi = 300, device = cairo_pdf)
plot_resid_qqplot(epac.fit.high) #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac_high_resid_qqplot.pdf", width = 4, height = 2.5, dpi = 300, device = cairo_pdf)


# Histograms of sample -------------------------------------
plot_boot_coefs_hist <- function(boot.data.low, boot.data.high, fit.low, fit.high, stat) {
	all.data <- rbind(
		cbind(boot.data.low, sst.class = "low"),
		cbind(boot.data.high, sst.class = "high")
	)

	gg <- ggplot(all.data) +
		aes_string(x = stat) +
		geom_histogram(aes(fill = sst.class, group = sst.class),
									 position = "identity", colour = "black", bins = 50, alpha = 0.5) +
		geom_vline(xintercept = mean(boot.data.low[[stat]]),
							 colour = "dodgerblue1", linetype = "dashed") +
		geom_vline(xintercept = mean(boot.data.high[[stat]]),
							 colour = "brown1", linetype = "dashed") +
		scale_fill_manual(values = c("high" = "brown1",
																 "low"  = "dodgerblue1")) +
		guides(linetype = guide_legend(order = 2, override.aes = list(linetype = c(1,5)))) +
		labs(y = "Count", x = paste("Bootstraped", eval(stat)), fill = "SST Class" ) +
		theme_bw()

	if (stat == "slope") {
		gg <- gg +
			geom_vline(xintercept = fit.low$coefficients[2],
								 colour = "dodgerblue1", linetype = "solid") +
			geom_vline(xintercept = fit.high$coefficients[2],
								 colour = "brown1", linetype = "solid")
	} else if (stat == "inter") {
		gg <- gg +
			geom_vline(xintercept = fit.low$coefficients[1],
								 colour = "dodgerblue1", linetype = "solid") +
			geom_vline(xintercept = fit.high$coefficients[1],
								 colour = "brown1", linetype = "solid")
	}
	return(gg)
}

# ggplot(natl.boot.low.data) +
# 	geom_histogram(aes(x = r2), binwidth = 0.005)

plot_boot_coefs_hist(natl.boot.low.data, natl.boot.high.data, natl.fit.low, natl.fit.high, "slope") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl_boot_slope.pdf", width = 4, height = 2.75, dpi = 300, device = cairo_pdf)
plot_dists_qqplot(natl.boot.low.data$slope, natl.boot.high.data$slope) + labs(x = "Boostrapped slope (low SST)", y = "Boostrapped slope (high SST)") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl_boot_slope_qq.pdf", width = 4, height = 2.75, dpi = 300, device = cairo_pdf)

plot_boot_coefs_hist(natl.boot.low.data, natl.boot.high.data, natl.fit.low, natl.fit.high, "inter") + labs(x = "Bootstrapped intercept") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl_boot_inter.pdf", width = 4, height = 2.75, dpi = 300, device = cairo_pdf)
plot_dists_qqplot(natl.boot.low.data$inter, natl.boot.high.data$inter) + labs(x = "Boostrapped intercept (low SST)", y = "Boostrapped intercept (high SST)") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl_boot_inter_qq.pdf", width = 4, height = 2.75, dpi = 300, device = cairo_pdf)

plot_boot_coefs_hist(epac.boot.low.data, epac.boot.high.data, epac.fit.low, epac.fit.high, "slope") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac_boot_slope.pdf", width = 4, height = 2.75, dpi = 300, device = cairo_pdf)
plot_dists_qqplot(epac.boot.low.data$slope, epac.boot.high.data$slope) + labs(x = "Boostrapped slope (low SST)", y = "Boostrapped slope (high SST)") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac_boot_slope_qq.pdf", width = 4, height = 2.75, dpi = 300, device = cairo_pdf)

plot_boot_coefs_hist(epac.boot.low.data, epac.boot.high.data, epac.fit.low, epac.fit.high, "inter") #+ labs(x = "Bootstrapped intercept") + theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac_boot_inter.pdf", width = 4, height = 2.75, dpi = 300, device = cairo_pdf)
plot_dists_qqplot(epac.boot.low.data$inter, epac.boot.high.data$inter) + labs(x = "Boostrapped intercept (low SST)", y = "Boostrapped intercept (high SST)") #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac_boot_inter_qq.pdf", width = 4, height = 2.75, dpi = 300, device = cairo_pdf)



# Residual tests -------------------------------------------

perform_fit_residual_tests(natl.fit.low)
perform_fit_residual_tests(natl.fit.high)

perform_fit_residual_tests(epac.fit.low)
perform_fit_residual_tests(epac.fit.high)


# QQ Plots with bootstrap (residuals) ----------------------

natl.boot.low.resid <- get_boot_residuals(natl.data.low$storm.duration, natl.data.low$storm.pdi, natl.boot.low.stats)
plot_normal_qqplot(natl.boot.low.resid[["resid"]], "Residuals")


# Q-Q Plots (any variable) ---------------------------------
plot_normal_qqplot(natl.boot.low.data[["slope"]], "Slope")
plot_normal_qqplot(natl.boot.high.data[["slope"]], "Slope")

plot_normal_qqplot(natl.boot.low.data[["inter"]], "Intercept")
plot_normal_qqplot(natl.boot.high.data[["inter"]], "Intercept")

plot_normal_qqplot(epac.boot.low.data[["slope"]], "Slope")
plot_normal_qqplot(epac.boot.high.data[["slope"]], "Slope")

plot_normal_qqplot(epac.boot.low.data[["inter"]], "Intercept")
plot_normal_qqplot(epac.boot.high.data[["inter"]], "Intercept")
