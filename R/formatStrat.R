#' Lon Lat to km
#' 
#' Convert longitude and latitude in degrees to the same in kilometers
#' 
#' @param x longitude
#' @param y latitude
#' 
#' @details
#' Simple function to turn lon-lat distances (in degrees) into the kilometer equivalent. Given kilometers, easy to get areas, etc.
#' 
#' @return A named list with elements of Y (lat in km) and X (lon in km).
#' 
#' @export
ll2km <- function(x,y){
	if (!requireNamespace("PBSmapping", quietly = TRUE)) {
		stop("PBSmapping needed for this function to work. Please install it.", call. = FALSE)
	}
	blah <- data.frame(X=x, Y=y) 
	attr(blah, "projection")="LL"
	blah2 <- PBSmapping::convUL(blah)
	list(lat.km=blah2[,"Y"], lon.km=blah2[,"X"]) # returning a named list format is handy for use with data.table :=
}





# roundFrac <- function(x, frac=0.5){
# 	round(x/frac,0)*frac
# }
#' Round Grid
#' 
#' Round values to snap to a fraction of a grid
#' 
#' @param x numeric value to be rounded; can be a vector
#' @param frac numeric value representing the size of the grid
#' 
#' @details
#' If \code{frac} is 1, then round to the nearest whole number. If \code{frac} is 0.5, then snap everything to the nearest half a degree grid. If 10, then snap to the nearest multiple of 10, plus 5 (6 goes to 5, 8 goes to 5, 10 goes to 15, 21 goes to 25, etc). Handy if you have lat-lon data that you want to redefine as being on a grid.
#' 
#' @seealso \code{\link{ll2strat}}
#' 
#' @export
roundGrid <- function(x, frac=1){
	# if frac is 1, then place in a 1º grid
	# if frac is 0.5, then place in the 0.5º grid
	floor(x/frac)*frac+frac/2
}

#' Lon lat to strat
#' 
#' Convert longitude and latitude vectors to strata
#' 
#' @param lon numeric vector of longitude 
#' @param lat numeric vector of latitude
#' @param gridSize numeric indicating fraction for rounding
#' 
#' @details
#' \code{gridSize} is passed to \code{frac} in \code{\link{roundGrid}}. Defines a stratum based on rounding to the center of the nearest grid cell, of the specific size (\code{gridSize}).
#' 
#' @return
#' Character vector indicating the center of the defined stratum as the gridded lon and lat.
#' 
#' @export
ll2strat <- function(lon, lat, gridSize=1){
	do.call("paste", list(roundGrid(lon, gridSize), roundGrid(lat, gridSize)))
}


#' Calculate Area
#' 
#' Calculate the area of a region defined by a vector of lon-lat coordinates
#' 
#' @param lonlat a data.frame with numeric longitude in first column, latitude in second
#' 
#' @export
calcarea <- function(lonlat){
	if(!requireNamespace("PBSmapping", quietly = TRUE)){
		stop("PBSmapping needed for this function to work. Please install it.", call. = FALSE)
	}
	hullpts <- chull(x=lonlat[,1], y=lonlat[,2]) # find indices of vertices
	hullpts <- c(hullpts,hullpts[1]) # close the loop
	ps <- PBSmapping::appendPolys(NULL,mat=as.matrix(lonlat[hullpts,]),1,1,FALSE) # create a Polyset object
	attr(ps,"projection") <- "LL" # set projection to lat/lon
	psUTM <- PBSmapping::convUL(ps, km=TRUE) # convert to UTM in km
	polygonArea <- PBSmapping::calcArea(psUTM,rollup=1)
	return(polygonArea$area)
}

