#' Update Taxonomic Database
#' 
#' Update package database of species information
#' 
#' @param path default "data", path for location to save the .RData files
#' 
#' @return nothing, but saves .RData files
#' 
#' @details
#' Somewhat of a work in progress. The underlying functions called from here are complicated and clunky. Heavily relies on rtaxize package. The functions I wrote were originally for personal use, and I tried to program around doing an exhaustive search, but this made the functions too complicated for their own good. On the upside, they do a lot of \code{\link{tryCatch}} to search the different rtaxize databases, making the whole process more robust.
#' This full process is very slow, especially for large numbers of species. The species looked up are all of those in the raw data sets. The slowest step is by far the common names.
#' Other functionality, such as trophic level, would be nice and is possible. Similarly, other ecological information and images. Some of this could be accomplished with rfishbase. 
updateDB <- function(path="data"){
	# Function to search all taxonomy data bases for values in the trawl data set
	# It will take a very long time to run this script
	# There are 3 main tasks:
		# 1) Find all Spp names
		# 2) Look up taxonomic classification
		# 3) Obtain common names
	
	# ==========================
	# = Load The Species Names =
	# ==========================
	# load("data/tempo-image-spp.key.RData")
	ref <- sort(unique(c(
		trawlData::clean.ai[,unique(ref)], 
		trawlData::clean.ebs[,unique(ref)], 
		trawlData::clean.gmex[,unique(ref)], 
		trawlData::clean.goa[,unique(ref)], 
		trawlData::clean.neus[,unique(ref)], 
		trawlData::clean.newf[,unique(ref)], 
		trawlData::clean.sa[,unique(ref)], 
		trawlData::clean.sgulf[,unique(ref)], 
		trawlData::clean.shelf[,unique(ref)], 
		trawlData::clean.wcann[,unique(ref)], 
		trawlData::clean.wctri[,unique(ref)]
	)))
	
	spp <- sort(unique(c(
		trawlData::clean.ai[,unique(spp)], 
		trawlData::clean.ebs[,unique(spp)], 
		trawlData::clean.gmex[,unique(spp)], 
		trawlData::clean.goa[,unique(spp)], 
		trawlData::clean.neus[,unique(spp)], 
		trawlData::clean.newf[,unique(spp)], 
		trawlData::clean.sa[,unique(spp)], 
		trawlData::clean.sgulf[,unique(spp)], 
		trawlData::clean.shelf[,unique(spp)], 
		trawlData::clean.wcann[,unique(spp)], 
		trawlData::clean.wctri[,unique(spp)]
	)))


	# ======================
	# = Find All Spp Names =
	# ======================
	# From raw names
	getSppData <- getSpp(c.all(ref[!is.na(ref)]))
	makeAsciiChar(getSppData)
	save(getSppData, file=file.path(path,"getSppData.RData"))


	# ============================
	# = Taxonomic Classification =
	# ============================
	getTaxData <- getTax(spp.key[!is.na(spp),unique(spp)])
	makeAsciiChar(getTaxData)
	save(getTaxData, file=file.path(path,"getTaxData.RData"))


	# ================
	# = Common Names =
	# ================
	getCmmnData <- getCmmn(spp.key[!is.na(spp),unique(spp)])
	makeAsciiChar(getCmmnData)
	save(getCmmnData, file=file.path(path,"getCmmnData.RData"))
}