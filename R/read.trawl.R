read.trawl <- function(reg, ...){
	reg <- match.arg(reg, choices=c(
				"ai",
				"ebs",
				"gmex",
				"goa",
				"neus",
				"newf",
				"ngulf",
				"sa",
				"sgulf",
				"shelf",
				"wcann",
				"wctri"
			)
	)
	
	dat <- switch(reg,
		ai = read.ai(),
		ebs = read.ebs(),
		gmex = read.gmex(),
		goa = read.goa(),
		neus = read.neus(),
		newf = read.newf(),
		ngulf = read.ngulf(),
		sa = read.sa(),
		sgulf = read.sgulf(),
		shelf = read.shelf(),
		wcann = read.wcann(),
		wctri = read.wctri()
	)
	
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
		
		# uncomment lines below if you want to remove
		# leading or trailing whitespace
		# in the column names
		# setnames(ai.raw, names(ai.raw), gsub("^\\s* | \\s*$", "", names(ai.raw)))
		# setnames(ai.strata, names(ai.strata), gsub("^\\s* | \\s*$", "", names(ai.strata)))
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
	
	
	# # check for leading or trXling
# 	# whitespace in col names
# 	strata.spaces <- grepl("^\\s* | \\s*$", "", names(X.strata))
# 	raw.spaces <- grepl("^\\s* | \\s*$", "", names(X.raw))
# 	if(strata.spaces | raw.spaces){
# 		message("AI data files have column names with leading or trXliling whitespace")
#
# 		# uncomment lines below if you want to remove
# 		# leading or trXling whitespace
# 		# in the column names
# 		# setnames(X.raw, names(X.raw), gsub("^\\s* | \\s*$", "", names(X.raw)))
# 		# setnames(X.strata, names(X.strata), gsub("^\\s* | \\s*$", "", names(X.strata)))
# 	}
	
	
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
	
}

# ========
# = NEUS =
# ========
read.neus <- function(){
	
}


# ========
# = NEWF =
# ========
read.newf <- function(){
	
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
read.sa <- function(){
	
}

# =========
# = SGULF =
# =========
read.sgulf <- function(){
	
}

# =========
# = SHELF =
# =========
read.shelf <- function(){
	
}

# ==========
# = WC ANN =
# ==========
read.wcann <- function(){
	
}

# ==========
# = WC TRI =
# ==========
read.wctri <- function(){
	
}




