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
update.taxDB <- function(path="data"){
	# Function to search all taxonomy data bases for values in the trawl data set
	# It will take a very long time to run this script
	# There are 3 main tasks:
		# 1) Find all Spp names
		# 2) Look up taxonomic classification
		# 3) Obtain common names
	
	
	# ===============================
	# = Prepare Necessary Materials =
	# ===============================
	# preparation to be deleted
	# library(data.table)
	# library(LaF)
	# library(stringr)
	# library(bit64)
	# library(PBSmapping) # for calculating stratum areas
	# library(maptools) # for calculating stratum areas
	# library(Hmisc)
	# library(taxize)
	#
	# setwd("~/Documents/School&Work/pinskyPost/trawlData/")
	#
	# source("./R/read.trawl.R")
	# source("./R/clean.names.R")
	# source("./R/clean.format.R")
	# source("./R/clean.columns.R")
	# source("./R/helper-file.R")
	# source("./R/helper-misc.R")
	# source("./R/format-value.R")
	# source("./R/format-strat.R")
	# source("./R/tax-getSpp.R")
	# source("./R/tax-grb.spp1.R")
	# source("./R/tax-getTL.R")
	# source("./R/tax-getCmmn.R")
	# source("./R/tax-getTax.R")


	# ==========================
	# = Load The Species Names =
	# ==========================
	# load("data/tempo-image-spp.key.RData")
	spp <- sort(unique(c(
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


	# ======================
	# = Find All Spp Names =
	# ======================
	# From raw names
	getSppData <- getSpp(c.all(spp[!is.na(spp)]))
	save(getSppData, file=file.path(path,"getSppData.RData"))


	# ============================
	# = Taxonomic Classification =
	# ============================
	getTaxData <- getTax(spp.key[!is.na(spp),unique(spp)])
	save(getTaxData, file=file.path(path,"getTaxData.RData"))


	# ================
	# = Common Names =
	# ================
	getCmmnData <- getCmmn(spp.key[!is.na(spp),unique(spp)])
	save(getCmmnData, file=file.path(path,"getCmmnData.RData"))
}