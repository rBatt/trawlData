
read.trawl <- function(reg=c("ai", "ebs", "gmex", "goa", "neus", "newf", "ngulf", "sa", "sgulf", "shelf", "wcann", "wctri"), ...){
	reg <- match.arg(reg, several.ok=TRUE)
	
	# dots <- list(...)
	
	get.raw <- function(x){
		switch(x,
			ai = read.ai(),
			ebs = read.ebs(),
			gmex = read.gmex(),
			goa = read.goa(),
			neus = read.neus(),
			newf = read.newf(),
			ngulf = read.ngulf(),
			# sa = read.sa(dots[[x]]),
			sa = read.sa(),
			sgulf = read.sgulf(),
			shelf = read.shelf(),
			wcann = read.wcann(),
			wctri = read.wctri()
		)
	}
	
	if(length(reg)>1){
		dat <- lapply(reg, get.raw)
		names(dat) <- reg
	}else{
		stopifnot(length(reg)>=1)
		dat <- get.raw(reg)
	}
	
	
	return(dat)	
}


# ======
# = AI =
# ======
read.ai <- function(){
	
	
	# Read in all files from ai zip file
	# default pattern is a .csv, and ai is all .csv
	ai.all <- read.zip(zipfile="./inst/extdata/ai.zip", SIMPLIFY=F)
	
	
	# Concat raw ai data files
	# Have to make some assumption about which files 
	# If later I list exact names of files, can use those
	# instead of picking them by the number of columns
	file.ncol <- sapply(ai.all, function(x)ncol(x))
	data.ncol <- 17 # the main data files have 17 columns
	which.raw <- which(file.ncol==data.ncol) # list elements from main raw files
	ai.raw <- do.call(rbind, ai.all[which.raw])
	
	
	# read in stratum data file
	which.strat <- which(names(ai.all)=="ai-Strata.csv") # use which so next step can use [[]]
	ai.strata <- ai.all[[which.strat]][,list(StratumCode, Areakm2)] # subset ai.all, then choose 2 columns
	
	
	# check for leading or trailing
	# whitespace in col names
	strata.spaces <- grepl("^\\s* | \\s*$", "", names(ai.strata))
	raw.spaces <- grepl("^\\s* | \\s*$", "", names(ai.raw))
	if(strata.spaces | raw.spaces){
		message("AI data files have column names with leading or traililing whitespace")
	}
	
	
	# adjust strat column to ai.strata
	# to match that in ai.raw
	# so they can be merged
	setnames(ai.strata, "StratumCode", "STRATUM")
	
	
	# set keys
	setkey(ai.raw, STRATUM)
	setkey(ai.strata, STRATUM)
	
	
	# merge ai.raw and ai.strata
	ai <- merge(ai.raw, ai.strata, all.x=TRUE)
	
	
	return(ai)
}


# =======
# = EBS =
# =======
read.ebs <- function(){

	# Read in all files from X zip file
	# default pattern is a .csv, and X is all .csv
	X.all <- read.zip(zipfile="./inst/extdata/ebs.zip", SIMPLIFY=F)
	
	
	# Fix whitespace in column names
	# necessary for combining files
	X.all <- lapply(X.all, function(x)setnames(x, names(x), gsub("^\\s* | \\s*$", "", names(x))))
	
	
	# Concat raw X data files
	# Have to make some assumption about which files 
	# If later I list exact names of files, can use those
	# instead of picking them by the number of columns
	file.ncol <- sapply(X.all, function(x)ncol(x))
	data.ncol <- 17 # the mXn data files have 17 columns
	which.raw <- which(file.ncol==data.ncol) # list elements from mXn raw files
	X.raw <- do.call(rbind, X.all[which.raw])
	
	
	# read in stratum data file
	which.strat <- which(names(X.all)=="ebs-Strata.csv") # use which so next step can use [[]]
	X.strata <- X.all[[which.strat]][,list(StratumCode, Areakm2)] # subset X.all, then choose 2 columns
	
	
	# adjust strat column to X.strata
	# to match that in X.raw
	# so they can be merged
	setnames(X.strata, "StratumCode", "STRATUM")
	
	
	# set keys
	setkey(X.raw, STRATUM)
	setkey(X.strata, STRATUM)
	
	
	# merge X.raw and X.strata
	X <- merge(X.raw, X.strata, all.x=TRUE)
	
	
	return(X)

}


# ========
# = GMEX =
# ========
read.gmex <- function(){
	
	# gmex files need to be read in separately
	# because need to do more work passing args to fread()
	
	# regex patterns for grabbing each file type
	patterns <- c("BGSREC", "STAREC", "INVREC", "NEWBIOCODESBIG", "CRUISES")
	
	
	# define colClasses for each file
	colClasses1 <- c(rep("integer",7), rep("character",3), rep("integer",2), "numeric", "numeric", "character", rep("character",3))
	colClasses2 <- NULL
	colClasses3 <- c(rep("integer",7), "character", "numeric","character", "integer", rep("character",3), rep("numeric", 12))
	colClasses4 <- c("integer", "character", "character", "integer", "integer", "character", "character")
	colClasses5 <- NULL
	
	
	# read in each file type 1 at a time
	X.bio <- read.zip(
		zipfile="./inst/extdata/gmex.zip", colClasses=colClasses1, pattern=patterns[1], 
		drop=c("SAMPLE_BGS", "NODC_BGS", "IS_SAMPLE", "TAXONID", "CNT"), 
		SIMPLIFY=F
	)[[1]]
	
	X.sta <- read.zip(
		zipfile="./inst/extdata/gmex.zip", colClasses=colClasses2, pattern=patterns[2], 
		select=c('STATIONID', 'CRUISEID', 'CRUISE_NO', 'P_STA_NO', 'TIME_ZN', 'TIME_MIL', 'S_LATD', 'S_LATM', 'S_LOND', 'S_LONM', 'E_LATD', 'E_LATM', 'E_LOND', 'E_LONM', 'DEPTH_SSTA', 'MO_DAY_YR', "TEMP_SSURF", "TEMP_BOT", 'VESSEL_SPD', 'COMSTAT'),
		SIMPLIFY=F
	)[[1]]
	
	X.tow <- read.zip(
		zipfile="./inst/extdata/gmex.zip", colClasses=colClasses3, pattern=patterns[3], 
		select=c('STATIONID', 'CRUISE_NO', 'P_STA_NO', 'INVRECID', 'GEAR_SIZE', 'GEAR_TYPE', 'MESH_SIZE', 'MIN_FISH', 'OP'), 
		SIMPLIFY=F
	)[[1]]
	
	X.spp <- read.zip(
		zipfile="./inst/extdata/gmex.zip", colClasses=colClasses4, pattern=patterns[4], 
		SIMPLIFY=F
	)[[1]]
	
	X.cruises <- read.zip(
		zipfile="./inst/extdata/gmex.zip", colClasses=colClasses5, pattern=patterns[5], 
		select=c("CRUISEID", "VESSEL", "TITLE"),
		SIMPLIFY=F
	)[[1]]
	
	
	
	# Subsetting, formatting, etc
	# that needs to be complete here
	# (non-optional) for 
	# effective formatting
	X.tow <- X.tow[GEAR_TYPE=="ST"]
	
	bad.gmex.CODE <- names(X.spp[,table(CODE)][X.spp[,table(CODE)] > 1])
	good.gmex.CODE <- names(X.spp[,table(CODE)][X.spp[,table(CODE)] <= 1])
	setkey(X.spp, CODE)
	X.spp <- X.spp[good.gmex.CODE]
	setnames(X.spp, "CODE", "BIO_BGS")


	# merge
	X <- merge(X.bio, X.tow, by=intersect(names(X.bio), names(X.tow)), all.x=TRUE)
	X <- merge(X, X.sta, by=intersect(names(X), names(X.sta)), all.x=TRUE)
	X <- merge(X, X.spp[,list(BIO_BGS, TAXONOMIC)], by="BIO_BGS", all.x=TRUE) 
	X <- merge(X, X.cruises, by=intersect(names(X), names(X.cruises)), all.x=TRUE)
	
	
	return(X)
	
}


# =======
# = GOA =
# =======
read.goa <- function(){
	

	# Read in all files from X zip file
	# default pattern is a .csv, and X is all .csv
	X.all <- read.zip(zipfile="./inst/extdata/goa.zip", SIMPLIFY=F)
	
	
	# Fix whitespace in column names
	# necessary for combining files
	X.all <- lapply(X.all, function(x)setnames(x, names(x), gsub("^\\s* | \\s*$", "", names(x))))
	
	
	# Concat raw X data files
	# Have to make some assumption about which files 
	# If later I list exact names of files, can use those
	# instead of picking them by the number of columns
	file.ncol <- sapply(X.all, function(x)ncol(x))
	data.ncol <- 17 # the mXn data files have 17 columns
	which.raw <- which(file.ncol==data.ncol) # list elements from mXn raw files
	X.raw <- do.call(rbind, X.all[which.raw])
	
	
	# read in stratum data file
	which.strat <- which(names(X.all)=="goa-Strata.csv") # use which so next step can use [[]]
	X.strata <- X.all[[which.strat]][,list(StratumCode, Areakm2)] # subset X.all, then choose 2 columns
	
	
	# adjust strat column to X.strata
	# to match that in X.raw
	# so they can be merged
	setnames(X.strata, "StratumCode", "STRATUM")
	
	
	# set keys
	setkey(X.raw, STRATUM)
	setkey(X.strata, STRATUM)
	
	
	# merge X.raw and X.strata
	X <- merge(X.raw, X.strata, all.x=TRUE)
	
	
	return(X)
	
}

# ========
# = NEUS =
# ========
read.neus <- function(){
	
	
	
	neus.strata <- fread("inst/extdata/neus/neus-neusStrata.csv", select=c('StratumCode', 'Areanmi2')) # Need neusStrata.csv file from Malin (18-Aug-2014)
	local({ # create a local environment to read in .RData, to ensure that other objects aren't overwritten

		load("inst/extdata/neus/neus-station.RData") # station
		load("inst/extdata/neus/neus-Survdat.RData") # survdat
		load("inst/extdata/neus/neus-SVSPP.RData") # spp

		# assign variables in global environment
		assign("neus.station", station, envir=environment(read.neus))
		assign("neus.survdat.raw", survdat, envir=environment(read.neus))
		assign("neus.spp", spp, envir=environment(read.neus))
	
	}) # end expressions to be carried out in new local environment

	# make changes to neus.strata
	setkey(neus.strata, StratumCode)
	setnames(neus.strata, "StratumCode", "STRATUM") # updates the key, too; this was done b/c data.table:::merege.data.table does not accept by.x and by.y

	# make changes to neus.survdat.raw
	# setkey(neus.survdat.raw, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)

	# Merge spp and strata into neus data.table
	neus <- neus.survdat.raw
	neus <- merge(neus, neus.spp, by="SVSPP") # add species names
	neus <- merge(neus, neus.strata, by="STRATUM", all.x=TRUE)
	neus <- merge(neus, neus.station, all.x=TRUE, by=c("STATION","STRATUM","CRUISE6"))
	
	# trim columns duplicated from merge 
	trim.autoColumn(neus)
	
	return(neus)
	
}


# ========
# = NEWF =
# ========
read.newf <- function(){
	
	# read survey info
	newf.surv1 <- read.zip("inst/extdata/newf.zip", pattern="surveys_table\\.csv", colClasses=rep("character", 16), drop=c("Comment"), SIMPLIFY=F)[[1]] #
	newf.surv2 <- read.zip("inst/extdata/newf.zip", pattern="surveys_table2009-2011\\.csv", colClasses=rep("character", 7), SIMPLIFY=F)[[1]] #

	# read strata
	# I can't figure out what I needed these for,
	# so for now I'm commenting-out this code
	# but this code does work
	# strat.widths <- c(3,4,4,3)
# 	strat <- read.zip("inst/extdata/newf.zip", pattern="stratum_areas", cols=strat.widths, use.fwf=T, SIMPLIFY=F)
# 	strat <- lapply(strat, function(x)setnames(x, names(x), c('stratum', 'area', 'maxdepth', 'nafo')))
	
	# read species information
	newf.spp <- read.zip("inst/extdata/newf.zip", pattern="GFSPCY\\.CODE", SIMPLIFY=F, drop="V1", , colClasses=c("character","integer", rep("character",2)))[[1]]
		
	# read main data files
	data.widths <- c(1, 2, 3, 3, 2, 2, 2, 2, 3, 2, 3, 3, 1, 1, 1, 1, 4, 3, 3, 1, 4, 4, 4, 4, 3, 3, 5, 5, 1, 4, 4, 6, 7, 5, 5, 2, 2) # column widths
	col.types <- c(recordtype="integer", vessel="integer", trip="integer", set="integer", yearl="integer", monthl="integer", dayl="integer", settype="integer", stratum="character", nafo="character", unitarea="character", light="double", winddir="double", windforce="double", sea="double", bottom="double", timel="character", duration="double", distance="double", operation="double", depth="character", depthmin="character", depthmax="character", depthbottom="character", surftemp="double", bottemp="double", latstart="character", lonstart="character", posmethod="double", gear="double", sppcode="double", num="double", wgt="double", latend="character", lonend="character", bottempmeth="double", geardevice="double")
	data.pattern <- "(199[23456789]|200[0123456789]|201[012])\\.DAT$" # pattern for main data files
	newf.names <- names(col.types) # column names
	newf <- read.zip("inst/extdata/newf.zip", pattern=data.pattern, SIMPLIFY=F, use.fwf=T, cols=data.widths, column_types=col.types, column_names=newf.names) # read data set
	newf <- do.call(rbind, newf) # combine into 1 data.table
	setnames(newf, names(newf), newf.names) # set names
	
	# merge main data set w/ species names
	newf[,sppcode:=as.integer(sppcode)]
	newf <- merge(newf, newf.spp, all.x=T, by="sppcode")


	# Use the "surv" files to add
	# the "seson" column to newf
	# doing this "formatting"/ "column addtion"
	# in the read file b/c it's only use for 
	# the surv files
	fall.surv1 <- c(
		'2GH - Stratified Random Bottom Trawl Survey - Campelen 1800', 
		'Fall - Stratified Random Bottom Trawl Survey - Campelen 1800'
	)
	fallseries <- c(
		newf.surv1[Series%in%fall.surv1, as.character(CRUISE)],
		newf.surv2[season=='fall', as.character(cruise)]
	)

	spring.surv1 <- c(
		'Annual 3P - Stratified Random Bottom Trawl Survey - Campelen 1800', 
		'Spring 3LNO - Stratified Random Bottom Trawl Survey - Campelen 1800'
	)
	springseries <- c(
		newf.surv1[Series%in%spring.surv1, as.character(CRUISE)],
		newf.surv2[season=="spring", as.character(cruise)]
	)
	
	cruiseid <- newf[,paste(vessel, formatC(trip, width=3, flag=0), sep='')]
	is.fall <- cruiseid %in% fallseries
	is.spring <- cruiseid %in% springseries
	
	newf.season <- rep(NA, nrow(newf))
	newf.season[is.fall] <- "fall"
	newf.season[is.spring] <- "spring"

	newf[, season:=newf.season]
	

	return(newf)
	
	
}

# =========
# = NGULF =
# =========
read.ngulf <- function(){
	message("Function not ready yet")
}


# ======
# = SA =
# ======
read.sa <- function(catch=c("sa-Coastalbiomass.csv","sa-Coastalindividual.csv","sa-Coastallength.csv")){
	catch <- match.arg(catch)
	
	
	sa.all <- read.zip("inst/extdata/sa.zip", SIMPLIFY=F)
	sa.all.names1 <- names(sa.all[[catch]])
	sa.all.keep1 <- sa.all.names1[!grepl("^V[0-9]*$", sa.all.names1)]
	sa.all[[catch]] <- sa.all[[catch]][,eval(s2c(sa.all.keep1))]
	
	sa.mass <- sa.all[[catch]]
	sa.strat <- sa.all[["sa-CoastalEvent.csv"]]
	
	# adjustments needed for merge
	sa.mass[,COLLECTIONNUMBER:=as.character(COLLECTIONNUMBER)]
	sa.strat <- sa.strat[!duplicated(COLLECTIONNUMBER)] # not needed for catch 1

	# merge 
	sa <- merge(sa.mass, sa.strat, all.x=T, by=c("COLLECTIONNUMBER"))
	
	# trim columns duplicated from merge
	trim.autoColumn(sa)
	
	return(sa)
}

# =========
# = SGULF =
# =========
read.sgulf <- function(){
	
	# read
	X.all <- read.zip("inst/extdata/sgulf.zip", SIMPLIFY=F)
	X.catch <- X.all[["sgulf-southern Gulf survey data.csv"]]
	X.set <- X.all[[names(X.all)[grepl("sgulf-sGSL_RV Survey sets.*", names(X.all))]]]
	X.strata <- X.all[["sgulf-4T_RV_strata.csv"]]


	# merge
	sgulf <- merge(X.catch, X.set, all.x=TRUE, all.y=FALSE, by=c("vessel","cruise","year","set"))
	trim.autoColumn(sgulf) # remove redundant columns
	setnames(sgulf, "strat", "stratum")# formatting needed for merge
	sgulf <- merge(sgulf, X.strata, all.x=TRUE, by="stratum")
	
	return(sgulf)
	
}

# =========
# = SHELF =
# =========
read.shelf <- function(){
	
	# read in
	X.all <- read.zip("inst/extdata/shelf.zip", SIMPLIFY=F)
	X.catch <- X.all[[names(X.all)[grepl("shelf-gscat.*",names(X.all))]]]
	X.set <- X.all[[names(X.all)[grepl("shelf-gsinf.*",names(X.all))]]]
	X.spp <- X.all[[names(X.all)[grepl("shelf-species list.*",names(X.all))]]]
	X.strata <- X.all[[names(X.all)[grepl("shelf-strata.*",names(X.all))]]]
	
	
	# format/ adjustment needed for merge
	setnames(X.catch, "SPEC","CODE")
	X.set[,REMARKS:=NULL]
	setnames(X.set, "STRAT", "stratum")
	X.strata[,stratum:=as.character(stratum)]
	
	
	# merge
	shelf <- merge(X.catch, X.set, all.x=TRUE, all.y=FALSE, by=c("MISSION","SETNO"))
	shelf <- merge(shelf, X.strata, all.x=TRUE, by="stratum")
	shelf <- merge(shelf, X.spp, all.x=TRUE, by="CODE")
	
	
	return(shelf)
	
}

# ==========
# = WC ANN =
# ==========
read.wcann <- function(){
	
	# read
	X.all <- read.zip("inst/extdata/wcann.zip", SIMPLIFY=F)
	X.fish <- X.all[[names(X.all)[grepl("wcann.+fish\\.csv",names(X.all))]]]
	X.haul <- X.all[[names(X.all)[grepl("wcann.+haul\\.csv",names(X.all))]]]
	X.invert <- X.all[[names(X.all)[grepl("wcann.+invert\\.csv",names(X.all))]]]
	

	# merge/ rbind
	X.invert[,setdiff(names(X.fish), names(X.invert)):=NA]
	X.catch <- rbind(X.fish[, names(X.invert), with=FALSE], X.invert)
	wcann <- merge(X.catch, X.haul, by=intersect(names(X.catch), names(X.haul)), all.x=TRUE)
	
	return(wcann)
	
}

# ==========
# = WC TRI =
# ==========
read.wctri <- function(){
	
	# read
	X.all <- read.zip("inst/extdata/wctri.zip", SIMPLIFY=F)
	X.catch <- X.all[["wctri-CATCHWCTRIALLCOAST.csv"]]
	X.haul <- X.all[["wctri-HAULWCTRIALLCOAST.csv"]]
	X.spp <- X.all[["wctri-RACEBASE_SPECIES.csv"]]
	
	# modifications to avoid conflicting columns
	X.haul[,AUDITJOIN:=NULL]
	X.spp[,AUDITJOIN:=NULL]
	
	# merge
	m.cols1 <- c("CRUISEJOIN", "HAULJOIN", "REGION", "VESSEL", "CRUISE", "HAUL")
	wctri <- merge(X.catch, X.haul, by=m.cols1, all.x=TRUE)
	wctri <- merge(wctri, X.spp, by="SPECIES_CODE", all.x=TRUE)
	
	return(wctri)
	
}




