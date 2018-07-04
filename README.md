# Tropical-cyclones
Continuation of my [Bachelor's thesis](https://github.com/aldomann/tropical-cyclones). 

New data base scripts:
 - `[+] writedb_hurdat2.R`: prepare the HURDAT2 datasets to work with the OISST database.
 - `[+] writedb_hurdat2_oisst.R`: populate the HURDAT2 data with SST data from OISST.
 - `[+] writedb_hurdat2_oisst_landfalls.R`: build data set focused on landfalls and in-land observations.
 
New base scripts: 
 - `[*] hurdat2_base.R`: add `record.id` column (contains information about landfalls).
 - `[+] oisst_base.R`: functions to extract SST data from OISST.
 - `[+] regression_base.R`: functions to perform regression analysis.
 - `[+] resampling_base.R`: functions to perform bootstrap and permutation tests on regression coefficients.
 - `[+] residuals_base.R`: functions to analyse residuals in the linear model.
 - `[+] distributions_base.R`: functions to analyse population distributions for hurricane observations.
 - `[+] geographic_base.R`: functions to analise the geographical track of hurricanes.

New scripts: 
 - `[+] correlation_analysis.R`: explore the correlation between hurricane and sea variables.
 - `[+] regression_bootstrap.R`: bootstrap to get confidence intervals of the slopes of linear regressions.
 - `[+] regression_permutation_tests.R`: analyse null hypotheses for linear regressions using permutation tests (and bootstrap).
 - `[+] residuals_analysis.R`: analyse residuals in the linear model.
 - `[+] distributions_analysis.R`: analyse population distributions for hurricane observations.
 - `[+] dev_systems.R`: analyse stationarity of developing and non-developing systems.
 - `[+] geographic_analysis.R`: analyse the influence of the geographical location on the duration of hurricanes.

Misc:
 - `[+] report_figures.R`: save various figures and print data into tables.
