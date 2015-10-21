clean.format <- function(X, reg=c("ai", "ebs", "gmex", "goa", "neus", "newf", "ngulf", "sa", "sgulf", "shelf", "wcann", "wctri")){
	
	reg <- match.arg(reg)
	
	
	clean.format0 <- function(x){
		switch(x,
			ai = clean.format.ai(X),
			ebs = clean.format.ebs(X),
			gmex = clean.format.gmex(X),
			goa = clean.format.goa(X),
			neus = clean.format.neus(X),
			newf = clean.format.newf(X),
			ngulf = clean.format.ngulf(X),
			sa = clean.format.sa(X),
			sgulf = clean.format.sgulf(X),
			shelf = clean.format.shelf(X),
			wcann = clean.format.wcann(X),
			wctri = clean.format.wctri(X)
		)
	}
	
	# ======================
	# = Fix Column Classes =
	# ======================
	# standard column names that should be numeric
	numeric.cols <- c("airtemp","areaswept","bdo", "bsalin", "btemp", "cnt", "cntcpue", "depth", "depth.end", "depth.max", "depth.min", "depth.start", "depth2.start", "effort", "lat", "lat.deg.end", "lat.deg.start", "lat.end", "lat.min.end", "lat.min.start", "lat.start", "length", "lon", "lon.deg.end", "lon.deg.start", "lon.end", "lon.min.end", "lon.min.start", "lon.start", "ssalin", "stemp", "stratumarea", "stratumarea2", "temperature", "towarea", "towdistance", "towduration", "towspeed", "weight", "wtcpue")
	
	# standard column names that should be character
	character.cols <- c("comment", "common", "cruise", "date", "date.end", "datetime", "day", "dayl", "geartype","genus", "haul", "haulid", "month", "monthl", "season", "set", "station", "station.comment", "stratum", "str", "survey.name", "time", "timel", "timezone", "towID", "vessel", "year", "yearl")
	# test <- copy(ai)
# 	X <- test
	has.nc <- names(X)[names(X)%in%numeric.cols]
	has.cc <- names(X)[names(X)%in%character.cols]
	# X[,c(has.nc):=eval(s2c(has.nc))]
	X[,c(has.nc):=lapply(eval(s2c(has.nc)), as.numeric)]
	X[,c(has.cc):=lapply(eval(s2c(has.cc)), as.character)]
	
	# Change factors to characters
	isfactor <- sapply(X, is.factor)
	if(any(isfactor)){
		has.fc <- names(X)[isfactor]
		invisible(X[,c(has.fc):=lapply(eval(s2c(has.fc)), as.character)])
	}
	
	# White Space & 9's to NA's
	rmWhite(X) # remove whitespace in the elements of each column
	rm9s(X) # check each column for 9999, and replace with NA
	
	
	
	
	clean.format0(reg)
	
}




# ======
# = AI =
# ======
clean.format.ai <- function(X){
	
	# Calculate a corrected longitude for Aleutians (all in western hemisphere coordinates)
	if(X[,any(lon>0)]){
		X[lon>0, lon:=(lon-360)]
	}
	
	
	

}


# =======
# = EBS =
# =======
clean.format.ebs <- function(X){



}


# ========
# = GMEX =
# ========
clean.format.gmex <- function(X){
	

	
}


# =======
# = GOA =
# =======
clean.format.goa <- function(X){
	


	
}

# ========
# = NEUS =
# ========
clean.format.neus <- function(X){
	
	
	
	
}


# ========
# = NEWF =
# ========
clean.format.newf <- function(X){
	

	
	
}

# =========
# = NGULF =
# =========
clean.format.ngulf <- function(X){
	message("Function not ready yet")
}


# ======
# = SA =
# ======
clean.format.sa <- function(X){

}

# =========
# = SGULF =
# =========
clean.format.sgulf <- function(X){
	

	
}

# =========
# = SHELF =
# =========
clean.format.shelf <- function(X){
	

	
}

# ==========
# = WC ANN =
# ==========
clean.format.wcann <- function(X){
	

	
}

# ==========
# = WC TRI =
# ==========
clean.format.wctri <- function(X){

	
}







