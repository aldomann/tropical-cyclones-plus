#!/usr/bin/awk -f

# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Legal Stuff:
#	This script is free software: you can redistribute it and/or modify
#	it under the terms of the GNU General Public License as published by
#	the Free Software Foundation, version 3.

#	This script is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#	GNU General Public License for more details.

#	You should have received a copy of the GNU General Public License
#	along with this script. If not, see <http://www.gnu.org/licenses/>.

BEGIN{
	FS = ","
	OFS = ""
	print "# Script to download all needed OISST files to populate the HURDAT2."
	print "# Author: Alfredo Hernández <aldomann.designs@gmail.com>"
}
{
	if (NR!=1){
		fileid = substr($4, 1, 10);
		folderid = substr($4, 1, 7);
		gsub("-","", fileid);
		gsub("-","", folderid);
		print "wget -nc ",
		      "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/access/avhrr-only/",
		      folderid,
		      "/avhrr-only-v2.",
		      fileid,
		      ".nc",
		      " ;"
	}
}
