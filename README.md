# Tropical-cyclones
Continuation of my [Bachelor's thesis](https://gitlab.com/aldomann/tropical-cyclones). 

### Abstract
For the North-Atlantic basin, it has been shown (Corral 2010) that the probability distribution of the so-called power-dissipation index ($PDI$, a rough estimation of released energy) is indeed affected by the anual and basin-wide averaged sea surface temperature (SST), displacing towards more extreme values on warm years (high SST). As the $PDI$ integrates (cubic) wind speed over tropical-cyclone lifetime, it is an open question where the $PDI$ increase comes from (higher speed, longer lifetime, or both).

Our empirical results show a remarkable correlation in the joint distribution of lifetime and $PDI$, and linear regression of the logarithmic variables yields a power-law relation between both. Statistical testing, by means of a permutation test, shows that this relation does not significantly depend on the SST. In other words, the wind speed of a tropical cyclone of a given lifetime will be the same (within statistical fluctuations) in cold and warm years. Nevertheless, the increase of TC lifetime with SST triggers an increase in wind speed as a by-product. Further analysis shows that the longer lifetimes are mainly due to a shift to South-East of the TC genesis point.

Our conclusions are compatible with the view of tropical cyclones as an activation process, in which, once the event has started, its intensity is kept in critical balance between attenuation and intensification (and so, higher SST does not trigger more intensification). Summarising, storms with the same lifetime should have the same speed and $PDI$, no matter the SST, as, once the cyclone is activated, the wind speed at each TC stage should not depend on the SST.

## Description of the scripts

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
