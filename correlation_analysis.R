# Code to analyse the correlation between hurricane and sea variables
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Libraries ------------------------------------------------
library(measurements) # Convert units

# Source base code -----------------------------------------
source("oisst_base.R")

# Correlation analysis -------------------------------------

hurr.all.obs.full <- fread("data/hurdat2-oisst-1981-2016.csv") %>%
	mutate(date.time = ymd_hms(date.time)) %>%
	filter(is.na(wind) != TRUE)

# Missing wind values
# system("awk '{if($9 == "NA") print $0}' FS="," hurdat2-oisst-1981-2016.csv")
# AL171981, AL011984, AL021984, AL091984, AL171984, AL041986, AL071986, AL081987, AL091987, AL091987
# We had the same problem on the bachelor's thesis (they are only, a priori, a problem for the PDI calculation)

# Add wind velocity and SST anomalies to the data frame
hurr.all.obs.anomalies <- hurr.all.obs.full %>%
	group_by(storm.id, storm.name) %>%
	mutate(wind.anomaly = mean(wind) - wind) %>%
	mutate(sst.anomaly = mean(sst) - sst)

# Add wind velocity and SST anomalies to the data frame
hurr.all.obs.anomalies.alt <- hurr.all.obs.full %>%
	group_by(storm.id, storm.name) %>%
	mutate(wind.anomaly = max(wind, na.rm = T) - wind) %>%
	mutate(sst.anomaly = max(sst, na.rm = T) - sst)


# Data visualisation ---------------------------------------

# Scatterplots
ggplot(hurr.all.obs.full) +
	geom_point(aes(y = wind, x =  sst),
						 size = 0.1) +
	labs(title = "v vs SST") +
	theme_bw() #+ theme(legend.position = c(0.13, 0.67)) #+ ggsave(filename = "1.pdf", width = 6.5, height = 4, dpi = 96, device = cairo_pdf)

ggplot(hurr.all.obs.full) +
	geom_point(aes(y = wind, x =  max(hurr.all.obs.full$sst, na.rm = TRUE) - sst),
						 size = 0.1) +
	labs(title = "v vs max(SST) - SST") +
	theme_bw() #+ theme(legend.position = c(0.13, 0.67)) #+ ggsave(filename = "1.pdf", width = 6.5, height = 4, dpi = 96, device = cairo_pdf)

ggplot(hurr.all.obs.anomalies) +
	geom_point(aes(y = wind.anomaly, x = sst.anomaly),
						 size = 0.1) +
	labs(title = "mean(v) - v vs mean(SST) - SST, for each hurricane") +
	theme_bw() #+ theme(legend.position = c(0.13, 0.67)) #+ ggsave(filename = "2.pdf", width = 6.5, height = 4, dpi = 96, device = cairo_pdf)

# Heatmaps
ggplot(hurr.all.obs.full) +
	stat_bin2d(aes(y = wind, x = sst)) +
	scale_fill_gradient(high = "red", low = "blue",
											limits = c(15,5000), trans = "log") +
	theme_bw() #+ theme(legend.position = c(0.13, 0.67)) #+ ggsave(filename = "3.pdf", width = 6.5, height = 4, dpi = 96, device = cairo_pdf)

ggplot(hurr.all.obs.anomalies) +
	stat_bin2d(aes(y = wind.anomaly, x = sst.anomaly)) +
	scale_fill_gradient(high = "red", low = "blue",
											limits = c(15,5000), trans = "log") +
	labs(title = "v - mean(v) vs SST - mean(SST), for each hurricane") +
	theme_bw() #+ theme(legend.position = c(0.13, 0.67)) #+ ggsave(filename = "4.pdf", width = 6.5, height = 4, dpi = 96, device = cairo_pdf)

ggplot(hurr.all.obs.anomalies.alt) +
	stat_bin2d(aes(y = wind.anomaly, x = sst.anomaly)) +
	scale_fill_gradient(high = "red", low = "blue",
											limits = c(15,5000), trans = "log") +
	labs(title = "v - mean(v) vs SST - mean(SST), for each hurricane") +
	theme_bw() #+ theme(legend.position = c(0.13, 0.67)) #+ ggsave(filename = "4.pdf", width = 6.5, height = 4, dpi = 96, device = cairo_pdf)


# Duplicate data? ------------------------------------------

# hurr.dup <- hurr.all.obs.full[duplicated(hurr.all.obs.full), ]
