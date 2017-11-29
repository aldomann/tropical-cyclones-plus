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
	mutate(wind.anomaly = wind - mean(wind)) %>%
	mutate(sst.anomaly = sst - mean(sst))

ggplot(hurr.all.obs.full, aes(x = conv_unit(wind, "knot", "m_per_sec"), y = sst)) +
	geom_point() +
	scale_x_log10() +
	scale_y_log10()

ggplot(hurr.all.obs.anomalies, aes(x = conv_unit(wind.anomaly, "knot", "m_per_sec"), y = sst.anomaly)) +
	geom_point() +
	scale_x_log10() +
	scale_y_log10()
