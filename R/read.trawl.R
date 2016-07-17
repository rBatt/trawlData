#' Read Trawl
#' 
#' Read in the raw trawl data sets for each region
#' 
#' @param reg the region name;  should be one of the following: \code{c("ai","ebs","gmex","goa", "neus", "newf", "sa", "sgulf", "shelf", "wcann", "wctri")}
#' @param zippath path to the zip file (or, for neus, to the folder containing a folder named 'neus')
#' @param ... nothing
#' 
#' @details
#' These functions read in raw data sets that, with the exception of \code{neus}, are in .zip files. An easy way to use these functions on the raw files included with the package is to do \code{setwd(system.file(package="trawlData"))}, then run the script as-is. However, you are equally able to run the function as you would any other read function (like \code{read.csv}, e.g.). Just remember that the function operates on a folder or zip file, not on a single data file.  
#' There is some use of regular expression to find appropriate file names. However, it is best to be sure that the files are named correctly (data files that come with this package are correctly named).
#' 
#' @examples
#' \dontrun{
#' ai.data <- read.trawl(reg="ai", zippath=file.path(system.file(package="trawlData"),"inst/extdata"))
#' }
#' 
#' @import data.table bit64
#' @export read.trawl
read.trawl <- function(reg=c("ai", "ebs", "gmex", "goa", "neus", "newf", "ngulf", "sa", "sgulf", "shelf", "wcann", "wctri"), zippath=file.path(system.file(package="trawlData"),"extdata"), ...){
	reg <- match.arg(reg, several.ok=TRUE)
	
	# dots <- list(...)
	
	get.raw <- function(x){
		switch(x,
			ai = read.alaskan(zippath, zipName="ai.zip", stratName="ai-Strata.csv"),
			ebs = read.alaskan(zippath, zipName="ebs.zip", stratName="ebs-Strata.csv"),
			gmex = read.gmex(zippath),
			goa = read.alaskan(zippath, zipName="goa.zip", stratName="goa-Strata.csv"),
			neus = read.neus(file.path(zippath,"neus")),
			newf = read.newf(zippath),
			ngulf = read.ngulf(zippath),
			# sa = read.sa(dots[[x]]),
			sa = read.sa(zippath),
			sgulf = read.sgulf(zippath),
			shelf = read.shelf(zippath),
			wcann = read.wcann(zippath),
			wctri = read.wctri(zippath)
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


# ==========================================================
# = Functions for all 3 Alaskan Regions (they are similar) =
# ==========================================================
read.alaskan_raw <- function(zippath, zipName=c("ai.zip","ebs.zip","goa.zip"), stratName=paste0(c("ai","ebs","goa"),"-Strata.csv")){
	zipName <- match.arg(zipName)
	stratName <- match.arg(stratName)
	
	# Read in all files from X zip file
	# default pattern is a .csv, and X is all .csv
	X.all <- read.zip(zipfile=file.path(zippath, zipName), SIMPLIFY=FALSE)
	
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
	which.strat <- which(names(X.all)==stratName) # use which so next step can use [[]]
	X.strata <- X.all[[which.strat]]
	
	return(list(X.raw=X.raw, X.strata=X.strata))
	
}

read.alaskan_merge <- function(alaskan_list){
	X.raw <- alaskan_list[['X.raw']]
	X.strata <- alaskan_list[['X.strata']][,list(StratumCode, Areakm2)]
	
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

read.alaskan <- function(zippath, zipName, stratName){
	alaskan_list <- read.alaskan_raw(zippath, zipName, stratName)
	alaskan_merge <- read.alaskan_merge(alaskan_list)
	return(alaskan_merge)
}


# ========
# = GMEX =
# ========
read.gmex_raw <- function(zippath){
	# gmex files need to be read in separately
	# because need to do more work passing args to fread()
	
	# regex patterns for grabbing each file type
	patterns <- c("BGSREC", "STAREC", "INVREC", "NEWBIOCODESBIG", "CRUISES")
	
	# # define colClasses for each file
	colClasses1 <- structure(c("integer", "integer", "character", "integer", "integer", "integer", "integer", "character", "character", "character", "integer", "numeric", "character"), .Names = c("BGSID", "CRUISEID", "STATIONID", "VESSEL", "CRUISE_NO", "P_STA_NO", "CATEGORY", "GENUS_BGS", "SPEC_BGS", "BGSCODE", "CNTEXP", "SELECT_BGS", "BIO_BGS"))
	colClasses2 <- structure(c("character", "numeric", "numeric", "numeric", "character", "character", "character", "numeric", "integer", "numeric", "numeric", "character", "integer", "numeric", "integer", "numeric", "numeric", "numeric", "numeric", "character"), .Names = c("STATIONID", "CRUISEID", "CRUISE_NO", "P_STA_NO", "TIME_ZN", "TIME_MIL", "S_LATD", "S_LATM", "S_LOND", "S_LONM", "DEPTH_SSTA", "MO_DAY_YR", "E_LATD", "E_LATM", "E_LOND", "E_LONM", "TEMP_SSURF", "TEMP_BOT", "VESSEL_SPD", "COMSTAT" ))
	colClasses3 <- structure(c("integer", "character", "integer", "integer", "integer", "integer", "integer", "character", "numeric", "character", "integer", "character", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), .Names = c("INVRECID", "STATIONID", "CRUISEID", "VESSEL", "CRUISE_NO", "P_STA_NO", "GEAR_SIZE", "GEAR_TYPE", "MESH_SIZE", "OP", "MIN_FISH", "WBCOLOR", "BOT_TYPE", "BOT_REG", "TOT_LIVE", "FIN_CATCH", "CRUS_CATCH", "OTHR_CATCH", "T_SAMPLEWT", "T_SELECTWT", "FIN_SMP_WT", "FIN_SEL_WT", "CRU_SMP_WT", "CRU_SEL_WT", "OTH_SMP_WT", "OTH_SEL_WT"))
	colClasses4 <- structure(c("integer", "character", "character", "integer", "integer", "character", "integer"), .Names = c("Key1", "TAXONOMIC", "CODE", "TAXONSIZECODE", "isactive", "common_name", "tsn"))
	colClasses5 <- structure(c("integer", "integer", "character", "integer", "integer", "character", "character", "character", "integer"), .Names = c("CRUISEID", "YR", "SOURCE", "VESSEL", "CRUISE_NO", "STARTCRU", "ENDCRU", "TITLE", "NOTE"))
	
	cc_all <- mget(paste0("colClasses",1:5))
	
	# read in each file type 1 at a time
	gmex_read <- function(pattern, CC=NA, lName=c("X.bio","X.sta","X.tow","X.spp","X.cruises")){
		lName <- match.arg(lName)
		rawRead <- read.zip(zipfile=file.path(zippath, "gmex.zip"), pattern=pattern, SIMPLIFY=F, colClasses=CC)[[1]]
		structure(list(rawRead), .Names=lName)
	}
	
	gmex_files <- c("X.bio","X.sta","X.tow","X.spp","X.cruises")
	gmex_out <- structure(mapply(gmex_read, pattern=patterns, CC=cc_all, lName=gmex_files), .Names=gmex_files)
	
	return(gmex_out)
}

read.gmex_merge <- function(gmex_list){
	
	bio_select <- names(gmex_list$X.bio)[!names(gmex_list$X.bio)%in%c("SAMPLE_BGS", "NODC_BGS", "IS_SAMPLE", "TAXONID", "CNT")]
	sta_select <- c('STATIONID', 'CRUISEID', 'CRUISE_NO', 'P_STA_NO', 'TIME_ZN', 'TIME_MIL', 'S_LATD', 'S_LATM', 'S_LOND', 'S_LONM', 'E_LATD', 'E_LATM', 'E_LOND', 'E_LONM', 'DEPTH_SSTA', 'MO_DAY_YR', "TEMP_SSURF", "TEMP_BOT", 'VESSEL_SPD', 'COMSTAT')
	tow_select <- c('STATIONID', 'CRUISE_NO', 'P_STA_NO', 'INVRECID', 'GEAR_SIZE', 'GEAR_TYPE', 'MESH_SIZE', 'MIN_FISH', 'OP')
	cruises_select <- c("CRUISEID", "VESSEL", "TITLE")
	
	X.bio <- gmex_list$X.bio[,eval(s2c(bio_select))]
	X.sta <- gmex_list$X.sta[,eval(s2c(sta_select))]
	X.tow <- gmex_list$X.tow[,eval(s2c(tow_select))]
	X.spp <- gmex_list$X.spp
	X.cruises <- gmex_list$X.cruises[,eval(s2c(cruises_select))]
	
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

read.gmex <- function(zippath){
	gmex_list <- read.gmex_raw(zippath)
	gmex_merge <- read.gmex_merge(gmex_list)
	return(gmex_merge)
}


# ========
# = NEUS =
# ========
read.neus_raw <- function(zippath){
	X.strata <- fread(file.path(zippath, "neus-neusStrata.csv"), select=c('StratumCode', 'Areanmi2')) # Need neusStrata.csv file from Malin (18-Aug-2014)
	local({ # create a local environment to read in .RData, to ensure that other objects aren't overwritten

		load(file.path(zippath, "neus-station.RData")) # station
		load(file.path(zippath, "neus-Survdat.RData")) # survdat
		load(file.path(zippath, "neus-SVSPP.RData")) # spp

		# assign variables in global environment
		assign("X.station", station, envir=environment(read.neus_raw))
		assign("X.raw", survdat, envir=environment(read.neus_raw))
		assign("X.spp", spp, envir=environment(read.neus_raw))
	
	}) # end expressions to be carried out in new local environment
	
	return(list(X.raw=X.raw, X.spp=X.spp, X.station=X.station, X.strata=X.strata))
}

read.neus_merge <- function(neus_list){
	# make changes to neus.strata
	setkey(neus_list$X.strata, StratumCode)
	setnames(neus_list$X.strata, "StratumCode", "STRATUM") # updates the key, too; this was done b/c data.table:::merege.data.table does not accept by.x and by.y
	
	# LAT exists in both neus.station and neus.survdat.raw
	# my checks show that they don't match; I'm going to go with the neus.survdat.raw version
	# when I look carefully, some different by -6E-15 or 6E-15 ... it's just rounding
	neus_list$X.station[,LAT:=NULL]

	# Merge spp and strata into neus data.table
	X <- neus_list$X.raw
	X <- merge(X, neus_list$X.spp, by="SVSPP") # add species names
	X <- merge(X, neus_list$X.strata, by="STRATUM", all.x=TRUE)
	X <- merge(X, neus_list$X.station, all.x=TRUE, by=c("STATION","STRATUM","CRUISE6"))
	trim.autoColumn(X) # trim columns duplicated from merge 
	
	return(X)
}

read.neus <- function(zippath){
	neus_list <- read.neus_raw(zippath)
	neus_merge <- read.neus_merge(neus_list)
	return(neus_merge)
}


# ========
# = NEWF =
# ========

read.newf_raw <- function(zippath){
	# read survey info
	newf.surv1 <- read.zip(file.path(zippath, "newf.zip"), pattern="surveys_table\\.csv", colClasses=rep("character", 16), SIMPLIFY=FALSE)[[1]]
	newf.surv1[,Comment:=NULL]
	newf.surv2 <- read.zip(file.path(zippath, "newf.zip"), pattern="surveys_table2009-2011\\.csv", colClasses=rep("character", 7), SIMPLIFY=FALSE)[[1]]

	# read strata
	# I can't figure out what I needed these for,
	# so for now I'm commenting-out this code
	# but this code does work
	# strat.widths <- c(3,4,4,3)
# 	strat <- read.zip("inst/extdata/newf.zip", pattern="stratum_areas", cols=strat.widths, use.fwf=TRUE, SIMPLIFY=F)
# 	strat <- lapply(strat, function(x)setnames(x, names(x), c('stratum', 'area', 'maxdepth', 'nafo')))
	
	# read species information
	newf.spp <- read.zip(file.path(zippath, "newf.zip"), pattern="GFSPCY\\.CODE", SIMPLIFY=FALSE, colClasses=c("character","integer", rep("character",2)))[[1]]
	newf.spp[,X:=NULL]
	
	# read main data files
	data.widths <- c(1, 2, 3, 3, 2, 2, 2, 2, 3, 2, 3, 3, 1, 1, 1, 1, 4, 3, 3, 1, 4, 4, 4, 4, 3, 3, 5, 5, 1, 4, 4, 6, 7, 5, 5, 2, 2) # column widths
	col.types <- c(recordtype="integer", vessel="integer", trip="integer", set="integer", yearl="integer", monthl="integer", dayl="integer", settype="integer", stratum="character", nafo="character", unitarea="character", light="double", winddir="double", windforce="double", sea="double", bottom="double", timel="character", duration="double", distance="double", operation="double", depth="character", depthmin="character", depthmax="character", depthbottom="character", surftemp="double", bottemp="double", latstart="character", lonstart="character", posmethod="double", gear="double", sppcode="double", num="double", wgt="double", latend="character", lonend="character", bottempmeth="double", geardevice="double")
	data.pattern <- "(199[23456789]|200[0123456789]|201[012])\\.DAT$" # pattern for main data files
	newf.names <- names(col.types) # column names
	newf <- read.zip(file.path(zippath, "newf.zip"), pattern=data.pattern, SIMPLIFY=FALSE, use.fwf=TRUE, cols=data.widths, column_types=col.types, column_names=newf.names) # read data set
	newf <- do.call(rbind, newf) # combine into 1 data.table
	setnames(newf, names(newf), newf.names) # set names
	
	# merge main data set w/ species names
	newf[,sppcode:=as.integer(sppcode)]
	
	return(list(newf=newf, newf.surv1=newf.surv1, newf.surv2=newf.surv2, newf.spp=newf.spp))
}

read.newf_merge <- function(newf_list){
	newf <- merge(newf_list$newf, newf_list$newf.spp, all.x=TRUE, by="sppcode")
	
	# Use the "surv" files to add the "seson" column to newf
	# Doing this "formatting"/ "column addtion" in the read file b/c it's only use for the surv files
	fall.surv1 <- c(
		'2GH - Stratified Random Bottom Trawl Survey - Campelen 1800', 
		'Fall - Stratified Random Bottom Trawl Survey - Campelen 1800'
	)
	fallseries <- c(
		newf_list$newf.surv1[Series%in%fall.surv1, as.character(CRUISE)],
		newf_list$newf.surv2[season=='fall', as.character(cruise)]
	)
	spring.surv1 <- c(
		'Annual 3P - Stratified Random Bottom Trawl Survey - Campelen 1800', 
		'Spring 3LNO - Stratified Random Bottom Trawl Survey - Campelen 1800'
	)
	springseries <- c(
		newf_list$newf.surv1[Series%in%spring.surv1, as.character(CRUISE)],
		newf_list$newf.surv2[season=="spring", as.character(cruise)]
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

read.newf <- function(zippath){
	newf_list <- read.newf_raw(zippath)
	newf_merge <- read.newf_merge(newf_list)
	return(newf_merge)
}


# =========
# = NGULF =
# =========
read.ngulf <- function(zippath){
	message("Function not ready yet")
}


# ======
# = SA =
# ======
read.sa_raw <- function(zippath, catch=c("sa-catch.csv", "sa-Coastalbiomass.csv","sa-Coastalindividual.csv","sa-Coastallength.csv")){
	catch <- match.arg(catch)
	
	sa.all <- read.zip(file.path(zippath, "sa.zip"), SIMPLIFY=F)
	sa.all.names1 <- names(sa.all[[catch]])
	sa.all.keep1 <- sa.all.names1[!grepl("^V[0-9]*$", sa.all.names1)]
	sa.all[[catch]] <- sa.all[[catch]][,eval(s2c(sa.all.keep1))]
	
	sa.mass <- sa.all[[catch]]
	sa.strat <- sa.all[["sa-CoastalEvent.csv"]]
	
	return(list(sa.mass=sa.mass, sa.strat=sa.strat))
}

read.sa_merge <- function(sa_list){
	# adjustments needed for merge
	# sa.mass[,COLLECTIONNUMBER:=as.character(COLLECTIONNUMBER)]
	sa.strat <- sa_list$sa.strat[!duplicated(COLLECTIONNUMBER)] # not needed for catch 1
	sa.strat[,c("DATE","LASTUPDATED"):=NULL]

	# merge 
	sa <- merge(sa_list$sa.mass, sa.strat, all.x=TRUE, by=c("COLLECTIONNUMBER"))
	
	# trim columns duplicated from merge
	trim.autoColumn(sa)
	
	return(sa)
}

read.sa <- function(zippath){
	sa_list <- read.sa_raw(zippath)
	sa_merge <- read.sa_merge(sa_list)
	return(sa_merge)
}


# =========
# = SGULF =
# =========
read.sgulf_raw <- function(zippath){
	X.all <- read.zip(file.path(zippath, "sgulf.zip"), SIMPLIFY=F)
	X.catch <- X.all[["sgulf-southern Gulf survey data.csv"]]
	X.set <- X.all[[names(X.all)[grepl("sgulf-sGSL_RV Survey sets.*", names(X.all))]]]
	X.strata <- X.all[["sgulf-4T_RV_strata.csv"]]
	return(list(X.catch=X.catch, X.set=X.set, X.strata=X.strata))
}

read.sgulf_merge <- function(sgulf_list){
	sgulf <- merge(sgulf_list$X.catch, sgulf_list$X.set, all.x=TRUE, all.y=FALSE, by=c("vessel","cruise","year","set"))
	trim.autoColumn(sgulf) # remove redundant columns
	setnames(sgulf, "strat", "stratum")# formatting needed for merge
	sgulf <- merge(sgulf, sgulf_list$X.strata, all.x=TRUE, by="stratum")	
	return(sgulf)
}

read.sgulf <- function(zippath){
	sgulf_list <- read.sgulf_raw(zippath)
	sgulf_merge <- read.sgulf_merge(sgulf_list)
	return(sgulf_merge)
}


# =========
# = SHELF =
# =========
read.shelf_raw <- function(zippath){
	X.all <- read.zip(file.path(zippath, "shelf.zip"), SIMPLIFY=FALSE)
	namesX <- names(X.all)
	X.catch <- X.all[[namesX[grepl("shelf-gscat.*",namesX)]]]
	X.set <- X.all[[namesX[grepl("shelf-gsinf.*",namesX)]]]
	X.spp <- X.all[[namesX[grepl("shelf-species list.*",namesX)]]]
	X.strata <- X.all[[namesX[grepl("shelf-strata.*",namesX)]]]
	return(list(X.catch=X.catch, X.set=X.set, X.spp=X.spp, X.strata=X.strata))
}

read.shelf_merge <- function(shelf_list){
	# format/ adjustment needed for merge
	setnames(shelf_list$X.catch, "SPEC","CODE")
	shelf_list$X.set[,REMARKS:=NULL]
	setnames(shelf_list$X.set, "STRAT", "stratum")
	shelf_list$X.strata[,stratum:=as.character(stratum)]
	
	# merge
	shelf <- merge(shelf_list$X.catch, shelf_list$X.set, all.x=TRUE, all.y=FALSE, by=c("MISSION","SETNO"))
	shelf <- merge(shelf, shelf_list$X.strata, all.x=TRUE, by="stratum")
	shelf <- merge(shelf, shelf_list$X.spp, all.x=TRUE, by="CODE")
	
	return(shelf)
}

read.shelf <- function(zippath){
	shelf_list <- read.shelf_raw(zippath)
	shelf_merge <- read.shelf_merge(shelf_list)
	return(shelf_merge)
}


# ==========
# = WC ANN =
# ==========
read.wcann_raw <- function(zippath){
	X.all <- read.zip(file.path(zippath, "wcann.zip"), colClasses=c(Trawl.Id="integer64"), SIMPLIFY=FALSE)
	namesX <- names(X.all)
	X.fish <- X.all[[namesX[grepl("wcann.+fish\\.csv",namesX)]]]
	X.invert <- X.all[[namesX[grepl("wcann.+invert\\.csv",namesX)]]]
	X.haul <- X.all[[namesX[grepl("wcann.+haul\\.csv",namesX)]]]
	return(list(X.fish=X.fish, X.invert=X.invert, X.haul=X.haul))
}

read.wcann_merge <- function(wcann_list){
	wcann_list$X.invert[,setdiff(names(wcann_list$X.fish), names(wcann_list$X.invert)):=NA]
	X.catch <- rbind(wcann_list$X.fish[, names(wcann_list$X.invert), with=FALSE], wcann_list$X.invert)
	by_cols <- intersect(names(X.catch), names(wcann_list$X.haul))
	wcann <- merge(X.catch, wcann_list$X.haul, by=by_cols, all.x=TRUE, all.y=FALSE)
	return(wcann)
}

read.wcann <- function(zippath){
	wcann_list <- read.wcann_raw(zippath)
	wcann_merge <- read.wcann_merge(wcann_list)
	return(wcann_merge)
}


# ==========
# = WC TRI =
# ==========
read.wctri <- function(zippath){
	
	# read
	X.all <- read.zip(file.path(zippath, "wctri.zip"), SIMPLIFY=F)
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




