# Tropical-cyclones
Continuation of my [Bachelor's thesis](https://github.com/aldomann/tropical-cyclones). 

New base scripts: 
 - `[*] hurdat_base.R`: add `record.id` column (contains information about landfalls).
 - `[+] oisst_base.R`: functions to extract SST data from OISST.
 - `[+] writedb_hurdat2.R`: prepare the HURDAT2 datasets to work with the OISST database.
 - `[+] writedb_hurdat2_oisst.R`: populate the HURDAT2 data with SST data from OISST.
 - `[+] writedb_hurdat2_oisst_landfalls.R`: build data set focused on landfalls and in-land observations.

New scripts: 
 - `[+] correlation_analysis.R`: explore the correlation between hurricane and sea variables.
 - `[+] slopes_analysis.R`: use different statistical tests to analise the slopes of PDI vs duration.
