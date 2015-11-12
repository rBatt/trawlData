#' Clean Format
#' 
#' Clean by fixing data entry errors, and standardizing value formats
#' 
#' @template X_reg
#' 
#' @details
#' It is this function that makes specific corrections for data entry errors. For example, in one region a tow duration of 3 should have been 30. In another region some of the \code{effort} values were entered as \code{0} or \code{NA}, but should have had a particular value.  
#' This function also ensures that longitude and latitude are in the same format among regions.  
#' Other data entry errors or necessary corrections are implemented here, too.
#' Dates are not thoroughly formatted here, except in some cases where getting a \code{year}, e.g., requires parsing values out of other columns. POSIX class dates not created.
#' 
#' @import data.table
#' @export clean.format
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
	# can't do the lat/lon.start/end as numeric because newf needs them to stay as character 
	# in order for their conversion to decimal units to be simple
	# numeric.cols <- c("airtemp","areaswept","bdo", "bsalin", "btemp", "cnt", "cntcpue", "depth", "depth.end", "depth.max", "depth.min", "depth.start", "depth2.start", "effort", "lat", "lat.deg.end", "lat.deg.start", "lat.end", "lat.min.end", "lat.min.start", "lat.start", "length", "lon", "lon.deg.end", "lon.deg.start", "lon.end", "lon.min.end", "lon.min.start", "lon.start", "ssalin", "stemp", "stratumarea", "stratumarea2", "temperature", "towarea", "towdistance", "towduration", "towspeed", "weight", "wtcpue")
	numeric.cols <- c("airtemp","areaswept","bdo", "bsalin", "btemp", "cnt", "cntcpue", "depth", "depth.end", "depth.max", "depth.min", "depth.start", "depth2.start", "effort", "lat", "lat.deg.end", "lat.deg.start", "lat.min.end", "lat.min.start", "length", "lon", "lon.deg.end", "lon.deg.start", "lon.min.end", "lon.min.start", "ssalin", "stemp", "stratumarea", "stratumarea2", "temperature", "towarea", "towdistance", "towduration", "towspeed", "weight", "wtcpue")
	
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
	
	
	
	
	invisible(clean.format0(reg))
	
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

	# nothing to add beyond generic

}


# ========
# = GMEX =
# ========
clean.format.gmex <- function(X){
	
	X[lat.deg.start==0, lat.deg.start:=NA_real_]
	X[lon.deg.start==0, lon.deg.start:=NA_real_]
	X[lat.deg.end==0, lat.deg.end:=NA_real_]
	X[lon.deg.end==0, lon.deg.end:=NA_real_]

	X[,depth:=depth*1.8288] # convert fathoms to meters

	X[towspeed==30, towspeed:=3] # fix typo according to Jeff Rester: 30 = 3
	
	X[,time:=gsub("(?<=\\d)([\\d]{2})(?=$)", ":\\1", time, perl=TRUE)]
	
	X[survey.name=='Summer SEAMAP Groundfish Suvey', survey.name:='Summer SEAMAP Groundfish Survey']
	
}


# =======
# = GOA =
# =======
clean.format.goa <- function(X){
	
	# no changes to be made beyond what's 
	# already done in generic
	
	
}

# ========
# = NEUS =
# ========
clean.format.neus <- function(X){
	
	X[,stratumarea:=stratumarea*3.429904] # convert square nautical miles to square kilometers
	
	X[,season:=tolower(season)]
	
	
}


# ========
# = NEWF =
# ========
clean.format.newf <- function(X){
	
	# The formatting below is for the 
	# "strata" data files in read.newf,
	# but I currently don't bother with it
	# because the information doesn't seem to be needed
	# # Convert square nautical miles to square meters
	# strat1$aream2 <- strat1$area*3429904
	# strat2$aream2 <- strat2$area*3429904
	# strat3$aream2 <- strat3$area*3429904
	# strat4$aream2 <- strat4$area*3429904
	#
	# # Trim out spaces in NAFO division names
	# strat1$nafo <- gsub(" ", "", strat1$nafo)
	# strat2$nafo <- gsub(" ", "", strat2$nafo)
	# strat3$nafo <- gsub(" ", "", strat3$nafo)
	# strat4$nafo <- gsub(" ", "", strat4$nafo)
	
	X[,year:=as.numeric(year)+1900]
	X[year<1950, year:=year+100]
	
	
	
	# # ==================
	# # = Format lat/lon =
	# # ==================
	# # first, fix latitude
	# lat.1 <- newf.raw[,(lat.start>0&lat.end>0)]
	# lat.2 <- newf.raw[,(lat.start>0&lat.end==0)]
	# # lat.3 <- newf.raw[,(latstart==0&latend>0)] # no instances of this case
	#
	# newf.raw[lat.1,lat:=(as.numeric(substr(lat.start, 1, 2)) + as.numeric(substr(lat.start, 3, 5))/600 + as.numeric(substr(latend, 1, 2)) + as.numeric(substr(latend, 3, 5))/600)/2]
	#
	# newf.raw[lat.2, lat:=as.numeric(substr(lat.start, 1, 2)) + as.numeric(substr(lat.start, 3, 5))/600]

	conv.newf.lat <- function(x){
		if(!is.character(x)){
			message("already converted newf lat; no changes made")
			return(x)
		}
		as.numeric(substr(x,1,2))+as.numeric(substr(x,3,5))/600
	}
	conv.newf.lon <- function(x){
		if(!is.character(x)){
			message("already converted newf lon; no changes made")
			return(x)
		}
		-(as.numeric(substr(x,1,2))+as.numeric(substr(x,3,5))/600)
	}
	X[, lat.start:=conv.newf.lat(lat.start)]
	X[, lat.end:=conv.newf.lat(lat.end)]
	X[, lon.start:=conv.newf.lon(lon.start)]
	X[, lon.end:=conv.newf.lon(lon.end)]



	# # fix longitude
# 	lon.1 <- newf.raw[,(lonstart>0&lonend>0)]
# 	lon.2 <- newf.raw[,(lonstart>0&lonend==0)]
#
# 	newf.raw[lon.1, lon:=-(as.numeric(substr(lonstart, 1, 2)) + as.numeric(substr(lonstart, 3, 5))/600 + as.numeric(substr(lonend, 1, 2)) + as.numeric(substr(lonend, 3, 5))/600)/2]
#
# 	newf.raw[lon.2, lon:=-(as.numeric(substr(lonstart, 1, 2)) + as.numeric(substr(lonstart, 3, 5))/600)]
#
	
	# ====================
	# = Fix temperatures =
	# ====================
	
	# Fix the surface temp
	fixT.surf <- X[,stemp >= 880 & !is.na(stemp)]
	X[fixT.surf, stemp:= -(stemp - 900)/10]

	fixT.surf2 <- X[,stemp < 880 & stemp > 0 & !is.na(stemp)]
	X[fixT.surf2, stemp:=stemp/10]
	
	# X[,summary(stempt)] # 379,007 NAs (of 383,710 rows): nearly all missing # Ryan gets 379,007 NA's too
	

	# Fix the bottom temp
	fixT.bot <- X[,btemp >= 880 & !is.na(btemp)]
	X[fixT.bot, btemp:= -(btemp - 900)/10]

	fixT.bot2 <- X[,btemp < 880 & btemp > 0 & !is.na(btemp)]
	X[fixT.bot2, btemp:=btemp/10]
	# summary(newf.raw$bottemp) # only 6459 NAs
	
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
	
	# =====================================================
	# = Fix terrible ="asd" convention used in 10 columns =
	# =====================================================
	# Fix catch first
	# names(sa.catch00)[grepl("[\"=]", head(sa.catch00)[1,])]
	X[,PROJECTNAME:=gsub("[\"=]", "", PROJECTNAME)]
	X[,PROJECTAGENCY:=gsub("[\"=]", "", PROJECTAGENCY)]
	X[,haulid:=gsub("[\"=]", "", haulid)]
	X[,GEARCODE:=gsub("[\"=]", "", GEARCODE)]
	X[,spp:=gsub("[\"=]", "", spp)]
	X[,common:=gsub("[\"=]", "", common)]
	X[,LOCATION:=gsub("[\"=]", "", LOCATION)]
	X[,REGION:=gsub("[\"=]", "", REGION)]
	X[,DEPTHZONE:=gsub("[\"=]", "", DEPTHZONE)]
	X[,stratum:=gsub("[\"=]", "", stratum)]

	# Fix strata column
	X[,haulid:=gsub("[\"=]", "", haulid)]
	
	
	# ================================
	# = Fix lat/lon, add stratumarea =
	# ================================
	# this was weird, so I took a guess
	# Morley made similar corrections to this, but more direct rather than inferential
	# End result is same 
	# But see Morley's corrections: 
	# https://github.com/mpinsky/OceanAdapt/blob/master/complete_r_script.R#L225-L348
	X[lon.start > -782.000 & lon.start < -780.000, lon.start:=-79.0]
	X[lon.end > -782.000 & lon.end < -780.000, lon.end:=-78.988]
	X[lon.start > -802.000 & lon.start < -800.000, lon.start:= -81.006+0.01]
	X[lat.end>40, lat.end:=lat.start+0.009]
	
	
	# ==============
	# = Fix Effort =
	# ==============
	# From Morley
	# https://github.com/mpinsky/OceanAdapt/blob/master/complete_r_script.R#L225-L348
	X[haulid == 19910105, effort:=1.71273]
	X[haulid == 19990065, effort:=0.53648]
	X[haulid == 20070177, effort:=0.99936]
	X[haulid == 19950335, effort:=0.9775]
	X[haulid == 20110393, effort:=1.65726]
	X[EVENTNAME == 2014325, effort:=1.755] # I get nothing for this EVENTNAME ...
	X[EVENTNAME == 1992219, effort:=1.796247]
	X[haulid == 19910423, effort:=0.50031]
	X[haulid == 20010431, effort:=0.25099]
	
	
	# ================================
	# = Fix Weights for Some Species =
	# ================================
	# Corrections from Morley
	# https://github.com/mpinsky/OceanAdapt/blob/master/complete_r_script.R#L225-L348
	X[haulid==20010106 & SPECIESCODE==8713050104, weight:=31.9] # roughtail stingray # NOTE! I Don't get any instances of this species on this haul ...
	X[(is.na(weight) | weight==0) & SPECIESCODE==5802010101, weight:=(cnt*1.9)] # horseshoe crabs
	X[haulid==19940236 & SPECIESCODE == 9002050101, weight:=204] # leatherback sea turtle
	X[(weight==0 | is.na(weight)) & SPECIESCODE==9002040101, weight:=46] # loggerhead
	
	
}

# =========
# = SGULF =
# =========
clean.format.sgulf <- function(X){
	
	X[,time:=gsub("(?<=\\d)([\\d]{2})(?=$)", ":\\1", time, perl=TRUE)]
	
}

# =========
# = SHELF =
# =========
clean.format.shelf <- function(X){
	
	X[,stratumarea:=stratumarea*1.852^2]
	
	date.fix <- function(x){
		as.POSIXct(gsub("-", "/", x), format="%y/%m/%d", tz="GMT")
	}
	X[,date:=date.fix(date)]
	X[,date.end:=date.fix(date.end)]
	
	X[,time:=gsub("(?<=\\d)([\\d]{2})(?=$)", ":\\1", time, perl=TRUE)]
	
	fix.ll <- function(x){
		as.numeric(substr(x,1,2))+as.numeric(substr(x,3,4))/60
	}
	X[,lat.start:=fix.ll(lat.start)]
	X[,lon.start:= -fix.ll(lon.start)]
	X[,lat.end:=fix.ll(lat.end)]
	X[,lon.end:= -fix.ll(lon.end)]
	
	
}

# ==========
# = WC ANN =
# ==========
clean.format.wcann <- function(X){
	
	# nothing to add beyond generic
	X[,year:=gsub("Cycle ", "", year)]
	
}

# ==========
# = WC TRI =
# ==========
clean.format.wctri <- function(X){
	
	# nothing to add beyond generic
	
}







