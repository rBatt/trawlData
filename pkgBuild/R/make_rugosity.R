#' Make Rugosity Data Set
#' 
#' Make rugosity data set for the trawlData package from a .csv of rugosity from Pinsky
#' 
#' @param file character string indicating the rugosity csv file
#' 
#' @export
make_rugosity <- function(file="~/Documents/School&Work/pinskyPost/trawlData/inst/extdata/rugosity.csv"){
	rugos <- fread(file)
	rugos <- rasterFromXYZ(rugos)
	return(rugos)
}

