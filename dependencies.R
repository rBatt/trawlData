devtools::use_package("data.table") # Basis for handling all data sets


devtools::use_package("LaF", "Suggests") # For reading in fwf files
devtools::use_package("bit64", "Suggests") # Data class for long integers in data.table

devtools::use_package("PBSmapping", "Suggests") # for calculating stratum areas
# devtools::use_package("maptools", "Suggests") # for calculating stratum areas
# devtools::use_package("Hmisc", "Suggests") # for calculating stratum areas

devtools::use_package("taxize", "Suggests") # for updating taxonomy databases
devtools::use_package("rfishbase", "Suggests") # for updating taxonomy databases

devtools::use_package("raster", "Suggests") # for gridded environmental data sets
