update.taxDB <- function(){
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
	load("data/tempo-image-spp.key.RData")


	# ======================
	# = Find All Spp Names =
	# ======================
	# From raw names
	getSppData <- getSpp(c.all(spp[!is.na(spp)]))
	save(getSppData, file="data/getSppData.RData")


	# ============================
	# = Taxonomic Classification =
	# ============================
	getTaxData <- getTax(spp.key[!is.na(spp),unique(spp)])
	save(getTaxData, file="data/getTaxData.RData")


	# ================
	# = Common Names =
	# ================
	getCmmnData <- getCmmn(spp.key[!is.na(spp),unique(spp)])
	save(getCmmnData, file="data/getCmmnData.RData")
}