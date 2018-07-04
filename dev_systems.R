# Code to analyse of time stationarity of non-developing systems
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

library(tidyverse)
library(GGally)

# Get RAW data ---------------------------------------------

# pdi.all <- as_tibble(data.table::fread('data/hurdat2-hadisst-1966-2016_pdis_with_month.csv')) %>%
pdi.all <- as_tibble(data.table::fread('data/hurdat2-hadisst-1966-2016_pdis.csv')) %>%
	mutate(storm.duration = measurements::conv_unit(storm.duration, "sec", "hr"))

pdi.natl <- pdi.all %>%
	dplyr::filter(basin == "NATL")
pdi.epac <- pdi.all %>%
	dplyr::filter(basin == "EPAC")

construct_data <- function(pdi.df) {
	df <- pdi.df %>%
		mutate(dev = ifelse(max.wind <= 33, "NDS", "DS")) %>%
		# group_by(storm.year, storm.month, basin, sst.class) %>%
		group_by(storm.year,basin, sst.class) %>%
		summarise(
			ds = sum(dev == "DS"),
			nds = sum(dev == "NDS"),
			total = ds + nds
		)
	return(df)
}

# Test -----------------------------------------------------

natl.storms <- construct_data(pdi.natl)%>%
	mutate(Dates = as.Date(as.character(paste(storm.year, 01, 01, sep = "-"), "%Y-%m-%d")))

epac.storms <- construct_data(pdi.epac)%>%
	mutate(Dates = as.Date(as.character(paste(storm.year, 01, 01, sep = "-"), "%Y-%m-%d")))

summarise_stationarity_tests <- function(df) {
	test_stationarity <- function(df) {
		nds <- aTSA::kpss.test(df$nds, output = F)[[1,3]]
		ds <- aTSA::kpss.test(df$ds, output = F)[[1,3]]
		total <- aTSA::kpss.test(df$total, output = F)[[1,3]]

		return(c(nds, ds, total))
	}

	results <- as_tibble(
		rbind(
			test_stationarity(df),
			test_stationarity(df %>% filter(sst.class == "low")),
			test_stationarity(df %>% filter(sst.class == "high"))
		)
	)

	results <- as_tibble(cbind(type = c("all", "low", "high"), results)) %>%
		dplyr::rename("nds" = V1, "ds" = V2, "total" = V3)

	return(results)
}

summarise_stationarity_tests(natl.storms)
summarise_stationarity_tests(epac.storms)


# Plots ----------------------------------------------------

natl.storms.bis <- natl.storms
natl.storms.bis$sst.class <- natl.storms.bis$sst.class %>%
	plyr::revalue(c("low" = "Low SST",
									"high" = "High SST"))

epac.storms.bis <- epac.storms
epac.storms.bis$sst.class <- epac.storms.bis$sst.class %>%
	plyr::revalue(c("low" = "Low SST",
									"high" = "High SST"))

gg.ts.natl <- ggplot(natl.storms.bis) +
	aes(x = Dates, group = sst.class) +
	geom_line(aes(y = ds, colour = "Developing")) +
	geom_line(aes(y = nds, colour = "Non-developing")) +
	geom_line(aes(y = total, colour = "All storms")) +
	facet_wrap(~sst.class) +
	labs(x = "Time (year)", y = "Storm count", colour = "System") +
	theme_bw() +
	theme(legend.position="bottom")

gg.ts.epac <- ggplot(epac.storms.bis) +
	aes(x = Dates, group = sst.class) +
	geom_line(aes(y = ds, colour = "Developing")) +
	geom_line(aes(y = nds, colour = "Non-developing")) +
	geom_line(aes(y = total, colour = "All storms")) +
	facet_wrap(~sst.class) +
	labs(x = "Time (year)", y = "Storm count", colour = "System") +
	theme_bw() +
	theme(legend.position="bottom")

gg.ts.natl #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "natl-storms-ts.pdf", width = 7, height = 2.5, dpi = 96, device = cairo_pdf)

gg.ts.epac #+ theme(text = element_text(family = "Palatino")) + ggsave(filename = "epac-storms-ts.pdf", width = 7, height = 2.5, dpi = 96, device = cairo_pdf)
