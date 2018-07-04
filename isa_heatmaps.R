library(tidyverse)
library(gplots)

# Load data ------------------------------------------------

x <- data.table::fread('data/hurdat2-oisst-1981-2016.csv')

# Heatmaps -------------------------------------------------

# Basic
ggplot(x, aes(x = sst, y = wind)) +
	stat_bin2d()

# Filtering
ggplot(x %>% filter(sst > 20), aes(x = sst, y = wind)) +
	stat_bin2d(bins=30)

# Changing colours
ggplot(x, aes(x = sst, y = wind)) +
	stat_bin2d(bins=30) +
	scale_fill_gradient(high="red", low="blue", trans="log")

ggplot(x %>% filter(sst >= 20), aes(x = sst, y = wind)) +
	stat_bin2d(bins=30) +
	scale_fill_gradient(high="red", low="blue", trans="log")

# Adding limits
ggplot(x , aes(x = sst, y = wind)) +
	stat_bin2d() +
	scale_fill_gradient(high="red", low="blue",
											trans="log", limits = c(15,10000))

# Using gplots (Isabel style)
hist2d(x$wind[x$sst>20],x$sst[x$sst>20],nbins=30)
hist2d(x$wind,x$sst,nbins=30)

# Lapply Functions -----------------------------------------

# Test
z <- tapply(x$sst[!is.na(x$sst)],as.factor(x$wind[!is.na(x$sst)]),mean)
plot(names(table(as.factor(x$wind[!is.na(x$sst)]))),z)
plot(names(table(as.factor(x$wind[!is.na(x$sst)]))),z,ylim=c(0,30))

# Plot 1
plot(x$wind,x$sst,col="grey")
points(names(table(as.factor(x$wind[!is.na(x$sst)]))),z,ylim=c(0,30),col="red")
lines(names(table(as.factor(x$wind[!is.na(x$sst)]))),z,ylim=c(0,30),col="red")

# Plot 2
plot(x$sst,x$wind,col="grey")
lines(z,names(table(as.factor(x$wind[!is.na(x$sst)]))),ylim=c(0,30),col="red")
z <- tapply(x$wind[!is.na(x$sst)],cut(x$sst[!is.na(x$sst)],breaks=100),mean)
lines(z,ylim=c(0,30),col="red")
z <- tapply(x$sst[!is.na(x$sst)],as.factor(x$wind[!is.na(x$sst)]),max)
lines(names(table(as.factor(x$wind[!is.na(x$sst)]))),z,ylim=c(0,30),col="blue")

# Correlation
cor(x$wind[!is.na(x$sst)],x$sst[!is.na(x$sst)])
