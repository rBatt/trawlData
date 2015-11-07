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
#' \code{gridSize} is passed to \code{frac} in \link{\code{roundGrid}}. Defines a stratum based on rounding to the center of the nearest grid cell, of the specific size (\code{gridSize}).
#' 
#' @return
#' Character vector indicating the center of the defined stratum as the gridded lon and lat.
#' 
#' @export
ll2strat <- function(lon, lat, gridSize=1){
	do.call("paste", list(roundGrid(lon, gridSize), roundGrid(lat, gridSize)))
}






# save tolerance: "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/stratTol/"
# save tolerance figures: "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/stratTolFigs"

# Function can operate in 1 of 2 ways
 # 1) don't save .txt or figures, don't display figures, don't ask for the tolerance (just read in from .txt file), but change stratum in data.table
 # 2) Figures of tolerance are saved, figures are displayed, .txt of tolerance is saved, and stratum is change in data.table
#' Make Strata
#' 
#' Function to make strata for a region, examing missingness
#' 
#' @param x a data.table of trawl data
#' @param regName the name of the region
#' @param doLots option to specify tolerance for missingness; otherwise reads in file for it
#' 
#' @section Warning:  
#' This function is not ready to be used. Saves figures, has hard-coded paths, looks for reference files outisde of package, etc.
#' 
makeStrat <- function(x, regName, doLots=NULL){
	
	stopifnot(is.data.table(x))
	
	tolLoc <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Results/stratTol/"
	figLoc <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/stratTolFigs/"
	tol.txt <- paste0(regName,"Tol.txt")
	
	if(is.null(doLots)){
		if(!tol.txt%in%list.files(tolLoc)){
			doLots <- TRUE
		}else{
			doLots <- FALSE
		}
	}
	
	if(!doLots & !tol.txt%in%list.files(tolLoc)){
		stop("cannot set doLots to FALSE b/c tolerance files not found")
	}
	
	
	# ==================
	# = Create Stratum =
	# ==================
	nyears <- x[,length(unique(year))]
	x[,strat2:=ll2strat(lon, lat)]

	
	if(doLots){
		# ===============
		# = Make Figure =
		# ===============
		lat.range <- x[,range(lat, na.rm=TRUE)]
		lon.range <- x[,range(lon, na.rm=TRUE)]
	
		nstrata <- c()
		nstrata.orig <- c()
		for(i in 0:(nyears-1)){
			nstrata[i+1] <- x[,sum(colSums(table(year, strat2)>0)>=(nyears-i))]
			nstrata.orig[i+1] <- x[,sum(colSums(table(year, stratum)>0)>=(nyears-i))]
		}
	
		# Initialize graphical device
		png(paste0(figLoc,paste0(regName,".StratTol.png")), width=7, height=8.5, res=150, units="in")
		layout(matrix(c(rep(1,3), rep(2,3), rep(1,3), rep(2,3), 3:8),ncol=3))
		par(mar=c(2.0,1.75,1,0.1), mgp=c(1,0.15,0), tcl=-0.15, ps=8, cex=1, family="Times")
	
		# Tolerance vs. Missingness Panels
		plot(0:(nyears-1), nstrata, type="o", xlab="threshold # years missing", ylab="# strata below threshold missingness", main="# strata vs. tolerance of missingness")
		lines(0:(nyears-1), nstrata.orig, type="o", col="red")
		legend("topleft", legend=c("original strata definition", "1 degree grid definition"), lty=1, pch=21, col=c("red","black"))
		image(x=x[,sort(unique(year))], y=x[,1:length(unique(strat2))], z=x[,table(year, strat2)>0], xlab="year", ylab="1 degree stratum ID", main="stratum presence vs. time; red is absent")

		# Tolerance Maps
		par(mar=c(1.25,1.25,0.1,0.1), mgp=c(1,0.15,0), tcl=-0.15, ps=8, cex=1, family="Times")
		tol0 <- x[strat2%in%x[,names(colSums(table(year, strat2)>0))[colSums(table(year, strat2)>0)>=(nyears-0)]]]
		tol0[,c("lat","lon"):=list(roundGrid(lat),roundGrid(lon))]
		for(i in 1:6){
			tolC <- x[strat2%in%x[,names(colSums(table(year, strat2)>0))[colSums(table(year, strat2)>0)>=(nyears-i)]]]
			tolC[,c("lat","lon"):=list(roundGrid(lat),roundGrid(lon))]
			setkey(tolC, lat, lon)
			tolC <- unique(tolC)
			tolC[,plot(lon, lat, xlab="", ylab="", xlim=lon.range, ylim=lat.range, col=1+(!paste(lon,lat)%in%tol0[,paste(lon,lat)]))]
			legend("topleft", paste("missing years =",i), inset=c(-0.1, -0.12), bty="n")
			
			tol0 <- tolC
		}
		dev.off()
	
	
		# ==========================================
		# = Determine and Save Extent of Tolerance =
		# ==========================================
		toleranceChoice <- as.integer(readline("How many years missing should be tolerated?"))
		write.table(cbind("region"=regName, "tolerance"=toleranceChoice), file=paste0(tolLoc,tol.txt), row.names=FALSE)
	
	}else{
		# ===============================
		# = Read in Extent of Tolerance =
		# ===============================
		toleranceChoice <- as.integer(read.table(file=paste0(tolLoc,tol.txt), header=TRUE)[,"tolerance"])
	}
	
	
	
	
	# ===================================
	# = Trim Strata (line 160 of malin) =
	# ===================================
	goodStrat2 <- x[,names(colSums(table(year, strat2)>0))[colSums(table(year, strat2)>0)>=(nyears-toleranceChoice)]]
	x <- x[strat2%in%goodStrat2]
	x[,stratum:=strat2]
	x[,strat2:=NULL]
	x
	
	
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
	polygonArea <- calcArea(psUTM,rollup=1)
	return(polygonArea$area)
}

