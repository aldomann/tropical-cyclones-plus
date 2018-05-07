# Code to analyse of time stability of non-developing systems
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

plot_dev_systems_ratio <- function(pdi.df) {
	df <- pdi.df %>%
	mutate(dev = ifelse(max.wind <= 33, "NDS", "DS")) %>%
	group_by(storm.year, basin) %>%
	summarise(
		ds = sum(dev == "DS"),
		nds = sum(dev == "NDS"),
		ratio = sum(dev == "NDS")/n()
		)

	gg <- ggplot(df) +
		geom_line(aes(x = storm.year, y = ratio, colour = basin)) +
		scale_y_continuous(limits = c(0, 1)) +
		labs(title = "Ratio of tropical depressions (v ≤ 33) per year") +
		scale_color_manual(values = c("EPAC" = "mediumseagreen", "NATL" = "mediumslateblue")) +
		theme_bw()

	return(gg)
}

plot_dev_systems_ratio(pdi.natl)
plot_dev_systems_ratio(pdi.epac)
plot_dev_systems_ratio(pdi.all) + facet_wrap(~ basin, nrow = 2)
