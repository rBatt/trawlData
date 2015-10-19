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
	
}


# ========
# = GMEX =
# ========
read.gmex <- function(){
	
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




