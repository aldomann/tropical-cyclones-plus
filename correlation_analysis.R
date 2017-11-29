# Code to analyse the correlation between hurricane and sea variables
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Source base code -----------------------------------------
source("oisst_base.R")

# Correlation analysis -------------------------------------

hurr.all.obs.full <- fread("data/hurdat2-oisst-1981-2016.csv")
