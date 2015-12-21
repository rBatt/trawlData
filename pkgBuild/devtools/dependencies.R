
update_dependencies <- function(){
	devtools::use_package("data.table", type="Depends") # Basis for handling all data sets


	devtools::use_package("LaF", "Imports") # For reading in fwf files
	devtools::use_package("bit64", "Imports") # Data class for long integers in data.table
	devtools::use_package("stringr", "Imports") # for rmWhite
	devtools::use_package("stringi", "Imports") # for converting to ASCII
	devtools::use_package("PBSmapping", "Imports") # for calculating stratum areas
	devtools::use_package("reshape2", "Imports")
	devtools::use_package("lubridate", "Imports") # for date formatting

	devtools::use_package("raster", "Suggests") # for gridded environmental data sets
	# devtools::use_package("maptools", "Suggests") # for calculating stratum areas
	# devtools::use_package("Hmisc", "Suggests") # for calculating stratum areas
	devtools::use_package("taxize", "Suggests") # for updating taxonomy databases
	devtools::use_package("rfishbase", "Suggests") # for updating taxonomy databases
	devtools::use_package("rgdal", "Suggests") # for plotting species
}



