# Code to populate the HURDAT2 data with SST data from OISST
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Source base code -----------------------------------------
source("oisst_base.R")

# Source OISST Functions -----------------------------------

# SRC: https://github.com/millerlp/Misc_R_scripts/blob/master/NOAA_OISST_ncdf4.R
source("NOAA_OISST_ncdf4.R")
mask <- "data/lsmask.oisst.v2.nc"

# Mine SST data from OISST ---------------------------------

# Download OISST data
if (file.exists("data/hurdat2-1981-2016.csv") & !file.exists("oisst-data/download_oisst_data.sh")) {
	system("awk -f get_oisst_urls.awk data/hurdat2-1981-2016.csv > oisst-data/download_oisst_data.sh")
	# Total time: 16h 45min
}

# Mine SSTs and write CSV
if (file.exists("data/hurdat2-1981-2016.csv") & !file.exists("data/hurdat2-oisst-1981-2016.csv")) {
	hurr.all.obs <- data.table::fread("data/hurdat2-1981-2016.csv")

	hurr.all.obs.new <- hurr.all.obs %>%
		mutate(date.time = ymd_hms(date.time))

	hurr.all.obs.full <- populate_sst(hurr.all.obs.new)
	# Elapsed time: 3.025 min

	write_csv(hurr.all.obs.full, "data/hurdat2-oisst-1981-2016.csv")
}

# write_csv(hurr.all.obs.full %>% filter(storm.id == "AL122005"), "data/katrina-oisst-clean.csv")
